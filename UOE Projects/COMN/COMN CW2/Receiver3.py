# Yubo Shao 2084333

import sys
from socket import *

# Read parameters from command line
serverPort = int(sys.argv[1])
fileName = sys.argv[2]

buf = 1024
EOF = False
expectedSEQ = 1
# Default ACK message
sndpkt = (0).to_bytes(2, 'big')

serverSocket = socket(AF_INET, SOCK_DGRAM)
serverSocket.setblocking(True)
serverSocket.bind(('', serverPort))

with open(fileName, 'wb') as file:
    while not EOF:
        data, clientAddress = serverSocket.recvfrom(buf + 3)
        SEQ = data[:2]
        # if expected packet received
        if int.from_bytes(SEQ, 'big') == expectedSEQ:
            # if EOF
            if data[2] != 1:
                file.write(data[3:])
                # update default ACK message to ACK the current packet
                sndpkt = expectedSEQ.to_bytes(2, 'big')
                serverSocket.sendto(sndpkt, clientAddress)
                expectedSEQ += 1
            else:
                file.write(data[3:])
                EOF = True
                sndpkt = expectedSEQ.to_bytes(2, 'big')
                # Send final ACK message multiple times to ensure it's received
                for i in range(15):
                    serverSocket.sendto(sndpkt, clientAddress)
        # Send current default ACK message
        serverSocket.sendto(sndpkt, clientAddress)

serverSocket.close()
sys.exit(0)
