# Yubo Shao 2084333

import sys
from socket import *

# Read parameters from command line
serverPort = int(sys.argv[1])
fileName = sys.argv[2]

# Read buffer (payload size)
buf = 1024
# Expected sequence number or acknowledgement number
ACK = 0

# Open UDP socket and bind to port
serverSocket = socket(AF_INET, SOCK_DGRAM)
serverSocket.bind(('', serverPort))

file = open(fileName, 'wb')

while True:
    # Read from socket
    data, clientAddress = serverSocket.recvfrom(buf + 3)
    # If received sequence number == expected sequence number
    if int.from_bytes(data[:2], 'big') == ACK:
        file.write(data[3:])
        ACK += 1
    # if current packet is end of file
    if data[2] == 1:
        break

file.close()
serverSocket.close()
sys.exit(0)
