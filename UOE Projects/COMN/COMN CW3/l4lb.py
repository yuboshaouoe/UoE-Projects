from ryu.base import app_manager
from ryu.controller import ofp_event
from ryu.controller.handler import CONFIG_DISPATCHER, MAIN_DISPATCHER
from ryu.controller.handler import set_ev_cls
from ryu.ofproto import ofproto_v1_4
from ryu.lib.packet import packet
from ryu.lib.packet import ethernet
from ryu.lib.packet import in_proto
from ryu.lib.packet import arp
from ryu.lib.packet import ipv4
from ryu.lib.packet import tcp
from ryu.lib.packet.tcp import TCP_SYN
from ryu.lib.packet.tcp import TCP_FIN
from ryu.lib.packet.tcp import TCP_RST
from ryu.lib.packet.tcp import TCP_ACK
from ryu.lib.packet.ether_types import ETH_TYPE_IP, ETH_TYPE_ARP

class L4Lb(app_manager.RyuApp):
    OFP_VERSIONS = [ofproto_v1_4.OFP_VERSION]

    def __init__(self, *args, **kwargs):
        super(L4Lb, self).__init__(*args, **kwargs)
        self.ht = {} # {(<sip><vip><sport><dport>): out_port, ...}
        self.vip = '10.0.0.10'
        self.dips = ('10.0.0.2', '10.0.0.3')
        self.dmacs = ('00:00:00:00:00:02', '00:00:00:00:00:03')
        self.cmac = ''
        self.prevDIP = self.dips[0]
        self.prevPort = 2
        self.prevMac = self.dmacs[0]

    def _send_packet(self, datapath, port, pkt):
        ofproto = datapath.ofproto
        parser = datapath.ofproto_parser
        pkt.serialize()
        data = pkt.data
        actions = [parser.OFPActionOutput(port=port)]
        out = parser.OFPPacketOut(datapath=datapath,
                                  buffer_id=ofproto.OFP_NO_BUFFER,
                                  in_port=ofproto.OFPP_CONTROLLER,
                                  actions=actions,
                                  data=data)
        return out

    @set_ev_cls(ofp_event.EventOFPSwitchFeatures, CONFIG_DISPATCHER)
    def features_handler(self, ev):
        dp = ev.msg.datapath
        ofp, psr = (dp.ofproto, dp.ofproto_parser)
        acts = [psr.OFPActionOutput(ofp.OFPP_CONTROLLER, ofp.OFPCML_NO_BUFFER)]
        self.add_flow(dp, 0, psr.OFPMatch(), acts)

    def add_flow(self, dp, prio, match, acts, buffer_id=None):
        ofp, psr = (dp.ofproto, dp.ofproto_parser)
        bid = buffer_id if buffer_id is not None else ofp.OFP_NO_BUFFER
        ins = [psr.OFPInstructionActions(ofp.OFPIT_APPLY_ACTIONS, acts)]
        mod = psr.OFPFlowMod(datapath=dp, buffer_id=bid, priority=prio,
                                match=match, instructions=ins)
        dp.send_msg(mod)

    @set_ev_cls(ofp_event.EventOFPPacketIn, MAIN_DISPATCHER)
    def _packet_in_handler(self, ev):
        msg = ev.msg
        in_port, pkt = (msg.match['in_port'], packet.Packet(msg.data))
        dp = msg.datapath
        ofp, psr, did = (dp.ofproto, dp.ofproto_parser, format(dp.id, '016d'))
        eth = pkt.get_protocols(ethernet.ethernet)[0]

        arph = pkt.get_protocols(arp.arp)
        iph = pkt.get_protocols(ipv4.ipv4)
        tcph = pkt.get_protocols(tcp.tcp)

        if eth.ethertype == ETH_TYPE_ARP:
            dst_ip, dst_mac, src_ip, src_mac = (arph[0].dst_ip, arph[0].dst_mac, arph[0].src_ip, arph[0].src_mac)
            #match = psr.OFPMatch(in_port=in_port, eth_type=eth.ethertype,
            #                     arp_spa=src_ip, arp_tpa=dst_ip, arp_sha=src_mac, arp_tha=dst_mac)
            if in_port == 1:
                self.cmac = src_mac
                e = ethernet.ethernet(src_mac, self.prevMac, ethertype=eth.ethertype)
                a = arp.arp_ip(arp.ARP_REPLY, self.prevMac, self.vip, src_mac, src_ip)
                p = packet.Packet()
                p.add_protocol(e)
                p.add_protocol(a)
                dp.send_msg(self._send_packet(dp, self.prevPort, p))
                self.prevDIP = self.dips[0] if self.prevDIP == self.dips[1] else self.dips[1]
                self.prevPort = 2 if self.prevPort == 3 else 3
                self.prevMac = self.dmacs[0] if self.prevDIP == self.dmacs[1] else self.dmacs[1]
                return

            elif in_port == 2:
                e = ethernet.ethernet(self.dmacs[0], self.cmac, ethertype=eth.ethertype)
                a = arp.arp_ip(arp.ARP_REPLY, self.cmac, dst_ip, self.dmacs[0], self.dips[0])
                p = packet.Packet()
                p.add_protocol(e)
                p.add_protocol(a)
                dp.send_msg(self._send_packet(dp, 1, p))
                return

            else:
                e = ethernet.ethernet(self.dmacs[1], self.cmac, ethertype=eth.ethertype)
                a = arp.arp_ip(arp.ARP_REPLY, self.cmac, dst_ip, self.dmacs[1], self.dips[1])
                p = packet.Packet()
                p.add_protocol(e)
                p.add_protocol(a)
                dp.send_msg(self._send_packet(dp, 1, p))
                return

        elif eth.ethertype == ETH_TYPE_IP:
            ip_src, ip_dst, s_port, d_port = (iph[0].src, iph[0].dst, tcph[0].src_port, tcph[0].dst_port)
            match = psr.OFPMatch(in_port=in_port, eth_type=eth.ethertype, ip_proto=in_proto.IPPROTO_TCP,
                                 ipv4_src=ip_src, ipv4_dst=ip_dst, tcp_src=s_port, tcp_dst=d_port)

            if in_port == 1:
                if tcph[0].has_flags(TCP_SYN, TCP_FIN) or \
                        tcph[0].has_flags(TCP_SYN, TCP_RST) or \
                        tcph[0].bits == 0:
                    acts = [psr.OFPActionOutput(ofp.OFPPC_NO_FWD)]
                else:
                    key = (ip_src, ip_dst, s_port, d_port)
                    acts = [psr.OFPActionSetField(ipv4_dst=self.prevDIP),
                            psr.OFPActionSetField(eth_dst=self.prevMac),
                            psr.OFPActionOutput(self.prevPort)]
                    if key not in self.ht.keys():
                        self.ht.update({key: self.prevPort})
                        self.add_flow(dp, 1, match, acts, msg.buffer_id)
                        self.prevDIP = self.dips[0] if self.prevDIP == self.dips[1] else self.dips[1]
                        self.prevPort = 2 if self.prevPort == 3 else 3
                        self.prevMac = self.dmacs[0] if self.prevDIP == self.dmacs[1] else self.dmacs[1]
                        if msg.buffer_id != ofp.OFP_NO_BUFFER:
                            return

            else:
                return_key = (ip_dst, self.vip, d_port, s_port)
                if return_key in self.ht.keys():
                    acts = [psr.OFPActionSetField(ipv4_src=self.vip),
                            psr.OFPActionOutput(1)]
                    self.add_flow(dp, 1, match, acts, msg.buffer_id)
                    if msg.buffer_id != ofp.OFP_NO_BUFFER:
                        return
                else:
                    acts = [psr.OFPActionOutput(ofp.OFPPC_NO_FWD)]

        else:
            acts = [psr.OFPActionOutput(ofp.OFPPC_NO_FWD)]

        data = msg.data if msg.buffer_id == ofp.OFP_NO_BUFFER else None
        out = psr.OFPPacketOut(datapath=dp, buffer_id=msg.buffer_id,
                               in_port=in_port, actions=acts, data=data)
        dp.send_msg(out)
