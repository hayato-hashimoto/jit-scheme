(import miscmacros)
(while* (not (eof-object? (read))) (display "echo.") (display it))
(display "end")
