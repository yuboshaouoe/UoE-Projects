import packet
import sys
import time
import os
from socket import *

# Read parameters from command line
remoteHost = sys.argv[1]
port = int(sys.argv[2])
fileName = sys.argv[3]
retryTimeout = int(sys.argv[4])

# Read buffer (payload size)
buf = 1024

SEQ = 0
EOF = 0
retransmissionCounter = 0

# Get file size for throughput calculations
fileSize = os.path.getsize(fileName)

# Open UPD socket and set timeout for retransmission
clientSocket = socket(AF_INET, SOCK_DGRAM)
clientSocket.settimeout(0.001 * retryTimeout)

file = open(fileName, "rb")

start = time.time()

while data := file.read(buf):
    # ACKed = is current packet acknowledged
    ACKed = False
    # check EOF flag
    EOF = 1 if len(data) < buf else 0
    pkt = packet.mkpkt(SEQ, EOF, data)
    clientSocket.sendto(pkt, (remoteHost, port))
    while not ACKed:
        try:
            ACK = int.from_bytes(clientSocket.recv(2), 'big')
            # If current packet received
            if ACK == SEQ:
                ACKed = True
                SEQ += 1
        except timeout:
            retransmissionCounter += 1
            # time out resend
            clientSocket.sendto(pkt, (remoteHost, port))

end = time.time()
file.close()
clientSocket.close()
# print number of retransmission and throughput
print(f"{retransmissionCounter} {fileSize / 1024 / (end - start)}")
sys.exit(0)
