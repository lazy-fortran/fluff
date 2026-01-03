program test_visual_columns
    use fluff_visual_columns, only: visual_columns
    use test_support, only: assert_equal_int
    implicit none

    call assert_equal_int(visual_columns(achar(9)//"x"), 5, "default tab width")
    call assert_equal_int(visual_columns(achar(9)//"x", tab_width=8), 9, &
                          "tab_width=8")
    call assert_equal_int(visual_columns(achar(9)//"x", tab_width=0), 5, &
                          "tab_width=0 fallback")
    call assert_equal_int(visual_columns(achar(9)//"x", tab_width=-3), 5, &
                          "tab_width negative fallback")

    call assert_equal_int(visual_columns("ab"//achar(9)//"c"), 5, &
                          "tab stop after two chars")
    call assert_equal_int(visual_columns("ab"//achar(9)//"c", tab_width=8), 9, &
                          "tab stop after two chars with width 8")

end program test_visual_columns
