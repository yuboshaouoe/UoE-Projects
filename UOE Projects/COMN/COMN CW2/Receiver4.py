# Yubo Shao 2084333

import sys
from socket import *

# Read parameters from command line
serverPort = int(sys.argv[1])
fileName = sys.argv[2]
rcvWindow = int(sys.argv[3])

buf = 1024
baseRCV = 1
EOF = False
# buffer for all acknowledged yet out-of-order packets
rcvPkts = {}

serverSocket = socket(AF_INET, SOCK_DGRAM)
serverSocket.setblocking(True)
serverSocket.settimeout(1)
serverSocket.bind(('', serverPort))

with open(fileName, 'wb') as file:
    while True:
        try:
            data, clientAddress = serverSocket.recvfrom(buf + 3)
            SEQByte = data[:2]
            SEQ = int.from_bytes(SEQByte, 'big')
            EOF = False if data[2] != 1 and not EOF else True

            # if received sequence number is in receiver window
            if baseRCV <= SEQ < (baseRCV + rcvWindow):
                if not EOF:
                    serverSocket.sendto(SEQByte, clientAddress)
                else:
                    for i in range(15):
                        serverSocket.sendto(SEQByte, clientAddress)

                # if packet not buffered, buffer it
                if SEQ not in set(rcvPkts.keys()):
                    rcvPkts[SEQ] = data[3:]

                # if packet for receiverBase is received
                if SEQ == baseRCV:
                    # write all consecutive buffered packets to file and delete them
                    consecutive = True
                    while consecutive:
                        if baseRCV in set(rcvPkts.keys()):
                            file.write(rcvPkts[baseRCV])
                            rcvPkts.pop(baseRCV)
                            baseRCV += 1
                        else:
                            consecutive = False

            # Resend ACK message for packets in previous receiver window
            if (baseRCV - rcvWindow) <= SEQ < baseRCV:
                serverSocket.sendto(SEQByte, clientAddress)

        except timeout:
            if EOF and not rcvPkts:
                break

serverSocket.close()
sys.exit(0)
