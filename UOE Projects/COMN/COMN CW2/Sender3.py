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

# buffer for all transmitted yet unacknowledged packets
packets = {}
timer = Timer(0.001 * retryTimeout)

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
        # Buffer the packet
        packets[nextSEQ] = packet.mkpkt(nextSEQ, EOF, data)
        clientSocket.sendto(packets[nextSEQ], (remoteHost, port))
        if baseSEQ == nextSEQ:
            timer.start()
        nextSEQ += 1
    try:
        ACK = int.from_bytes(clientSocket.recv(2), 'big')
        baseSEQ = ACK + 1
        # If sender caught up with receiver
        if baseSEQ == nextSEQ:
            timer.stop()
        # otherwise restart the timer to count down again
        else:
            timer.start()
    # if nothing received from socket
    except error:
        pass
    if timer.timeout():
        i = baseSEQ
        timer.start()
        # Resend all packets in window
        while i < nextSEQ:
            clientSocket.sendto(packets[i], (remoteHost, port))
            i += 1
    if EOF == 1 and nextSEQ == baseSEQ:
        break

end = time.time()

file.close()
print(fileSize / 1024 / (end - start))
clientSocket.close()
