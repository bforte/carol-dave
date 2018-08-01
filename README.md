# Carol & Dave

Carol & Dave is a minimalist esolang based on Brain-Flak: It replaces
the left stack and right stack by Carol's stack that behaves as usual
and Dave's queue.

Since Dave is a bit different he interprets 0 as truthy and anything else as
falsy, thus `{code}` will loop while 0 is the first element in the queue.

Since Carol and Dave are not as used to being the center of attention (unlike
[Alice & Bob](https://github.com/bforte/alice-bob)), they constantly fight for
it - resulting in a constantly swapping stack/queue.
