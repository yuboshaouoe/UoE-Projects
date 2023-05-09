sudo tc qdisc del dev lo root
sudo tc qdisc add dev lo root netem loss 5% delay 5ms rate 10mbit
