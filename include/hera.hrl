-record(measure, {
    seq = 0 :: non_neg_integer(),
    timestamp :: integer() | undefined,
    values = [] :: [number()]
}).

-type measure() :: #measure{}.