# Yubo Shao 2084333

# This class is a basic timer with custom duration (in seconds)
# able to start, stop, and check if the timer is currently running or if it timed out

import time


class Timer(object):
    TIMER_STOP = -1

    def __init__(self, duration):
        self._start_time = self.TIMER_STOP
        self._duration = duration

    def start(self):
        self._start_time = time.time()

    def stop(self):
        self._start_time = self.TIMER_STOP

    def running(self):
        return self._start_time != self.TIMER_STOP

    def timeout(self):
        return time.time() - self._start_time >= self._duration
