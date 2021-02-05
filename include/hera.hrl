-type timestamp() :: integer() | undefined.

-record(measure, {
    seq = 0 :: non_neg_integer(),
    timestamp :: timestamp(),
    values = [] :: [number()]
}).

-type measure() :: #measure{}.