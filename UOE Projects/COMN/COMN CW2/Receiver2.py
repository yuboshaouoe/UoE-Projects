# Yubo Shao 2084333

import sys
from socket import *

# Read parameters from command line
serverPort = int(sys.argv[1])
fileName = sys.argv[2]

# Read buffer (payload size)
buf = 1024
EOF = False
expectedSEQ = 0

# Open UDP socket and bind to port
serverSocket = socket(AF_INET, SOCK_DGRAM)
serverSocket.bind(('', serverPort))

with open(fileName, 'wb') as file:
    while not EOF:
        data, clientAddress = serverSocket.recvfrom(buf + 3)
        SEQ = data[:2]
        # if EOF send the final ACK multiple times
        if data[2] == 1:
            EOF = True
            for i in range(15):
                serverSocket.sendto(SEQ, clientAddress)
        else:
            serverSocket.sendto(SEQ, clientAddress)
        # if packet sequence number is correct
        if int.from_bytes(SEQ, 'big') == expectedSEQ:
            expectedSEQ += 1
            file.write(data[3:])

serverSocket.close()
sys.exit(0)
