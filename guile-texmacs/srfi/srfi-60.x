/* cpp arguments: srfi-60.c -DHAVE_CONFIG_H -I.. -I./.. -g -O2 -Wall -Wmissing-prototypes -Werror */
 scm_c_define_gsubr (s_scm_srfi60_log2_binary_factors, 1, 0, 0, (SCM (*)()) scm_srfi60_log2_binary_factors); ;
 scm_c_define_gsubr (s_scm_srfi60_copy_bit, 3, 0, 0, (SCM (*)()) scm_srfi60_copy_bit); ;
 scm_c_define_gsubr (s_scm_srfi60_rotate_bit_field, 4, 0, 0, (SCM (*)()) scm_srfi60_rotate_bit_field); ;
 scm_c_define_gsubr (s_scm_srfi60_reverse_bit_field, 3, 0, 0, (SCM (*)()) scm_srfi60_reverse_bit_field); ;
 scm_c_define_gsubr (s_scm_srfi60_integer_to_list, 1, 1, 0, (SCM (*)()) scm_srfi60_integer_to_list); ;
 scm_c_define_gsubr (s_scm_srfi60_list_to_integer, 1, 0, 0, (SCM (*)()) scm_srfi60_list_to_integer); ;
 scm_c_define_gsubr (s_srfi60_booleans_to_integer, 0, 0, 1, (SCM (*)()) scm_srfi60_list_to_integer); ;
