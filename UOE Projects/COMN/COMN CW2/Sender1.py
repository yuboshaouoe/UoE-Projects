# Yubo Shao 2084333

import sys
import time
from socket import *

# Read parameters from command line
remoteHost = sys.argv[1]
port = int(sys.argv[2])
fileName = sys.argv[3]

# Read buffer (payload size)
buf = 1024
# Initial sequence number and end of file flag
SEQ = 0
EOF = 0

# Open UDP socket
clientSocket = socket(AF_INET, SOCK_DGRAM)

file = open(fileName, "rb")
while True:
    data = file.read(buf)
    # Check if end of file and set flag
    EOF = 1 if len(data) < 1024 else 0
    # 2 bytes of sequence number + 1 byte of EOF flag + payload data
    clientSocket.sendto((SEQ.to_bytes(2, byteorder='big') +
                         EOF.to_bytes(1, byteorder='big') +
                         data),
                        (remoteHost, port))
    SEQ += 1
    # if EOF then break
    if EOF == 1:
        break
    # Space packet out to ensure correct packet order on arrival
    time.sleep(0.05)

clientSocket.close()
file.close()
sys.exit(0)
