## Line Counting 
> As performed in state-stack parsers

As the scanner parses token data, it tracks newline characters it encounters. At each newline character, it updates the `end_line_off` and `end_line_number` values, setting the first to the value of `scan_ptr` and increment the second by _1_.

When `set-tok` is called, the values of `end_line_off` and `end_line_number` are transferred to `chkp_line_off`  and `chkp_line_number`, respectively. At this point, we know have an accurate accounting of the number lines within the token and the offset of the last line contained within that token. 

When a token is shifted or skipped, the values of `start_line_off` and `start_line_num` are recorded in the token prior to issuing the relevant parse action. Once the action is issued, and before resuming parsing, the values of `(start|end)_line_off` and `(start|end)_line_num` are set to the equivalent `end_line_*` values. At this point the while process is repeated. 

