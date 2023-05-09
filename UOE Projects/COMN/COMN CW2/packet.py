# Yubo Shao 2084333

# The helper class for common methods across different protocols

# Makes a new packet given its sequence number, end of file flag, and payload
def mkpkt(SEQ, EOF, payload):
    return SEQ.to_bytes(2, byteorder='big', signed=False) + \
           EOF.to_bytes(1, byteorder='big', signed=False) + \
           payload
