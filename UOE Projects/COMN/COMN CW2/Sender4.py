# Yubo Shao 2084333

import sys
import time
import packet
import os
from timer import Timer
from socket import *

# Read parameters from command line
remoteHost = sys.argv[1]
port = int(sys.argv[2])
fileName = sys.argv[3]
retryTimeout = int(sys.argv[4])
windowSize = int(sys.argv[5])

# SendBase and NextSeqNum
baseSEQ = 1
nextSEQ = 1

EOF = 0
buf = 1024

# buffer for all transmitted yet unacknowledged packets and their timer
packets = {}

fileSize = os.path.getsize(fileName)

file = open(fileName, "rb")

clientSocket = socket(AF_INET, SOCK_DGRAM)
clientSocket.setblocking(False)

start = time.time()

while True:
    # if NextSeqNum is within window size
    if nextSEQ < baseSEQ + windowSize and EOF == 0:
        data = file.read(buf)
        EOF = 1 if len(data) < buf else 0
        pkt = packet.mkpkt(nextSEQ, EOF, data)
        timer = Timer(0.001 * retryTimeout)
        # Buffer packet and its own timer
        packets[nextSEQ] = (pkt, timer)
        clientSocket.sendto(pkt, (remoteHost, port))
        timer.start()
        nextSEQ += 1
    try:
        ACK = int.from_bytes(clientSocket.recv(2), 'big')
        # if received an ACK for a currently unACKed packet and packet is in window
        if ACK in set(packets.keys()) and baseSEQ <= ACK < (baseSEQ + windowSize):
            packets.pop(ACK)
        # if ACK == SendBase
        if ACK == baseSEQ:
            # Update SendBase to the smallest sent yet unacknowledged packet SEQ
            baseSEQ = list(packets.keys())[0] if list(packets.keys()) else nextSEQ
    except error:
        pass
    # If there is a timeout buffered packet, resend it
    for key in list(packets.keys()):
        pkt, timer = packets[key]
        if timer.timeout():
            clientSocket.sendto(pkt, (remoteHost, port))
            timer.start()
    # If EOF and all ACK are received
    if EOF == 1 and baseSEQ == nextSEQ:
        break

end = time.time()

file.close()
clientSocket.close()
print(fileSize / 1024 / (end - start))
