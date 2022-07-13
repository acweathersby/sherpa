@pragma once

struct ParseToken {
    unsigned int byte_offset;
    unsigned int cp_offset;
    unsigned int byte_length;
    unsigned int cp_length;
    unsigned int line_number;
    unsigned int line_offset;
    unsigned int garbage;
    unsigned int token_type;
};

template<class Reader>
struct ParseContext {
    struct ParseToken peek_token;
    struct ParseToken anchor_token;
    struct ParseToken assert_token;
    unsigned long long int foreign_rsp;
    unsigned long long int local_rsp // Points to top of local_state_stack;
    unsigned long long int local_stack_base;
    unsigned long long int state_u64_data;
    unsigned long long int action_pointer;
    Reader * reader;
    void * fn_get_line_data;
    void * fn_get_length_data;
    void * fn_next;
    void * fn_set_cursor_to;
    void * local_state_stack;

    
}