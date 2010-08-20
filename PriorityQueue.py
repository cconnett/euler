import bisect

class PriorityQueue(list):
    def _put(self, item):
        bisect.insort(self, item)
    put = _put

    def top(self):
        return self[0]
