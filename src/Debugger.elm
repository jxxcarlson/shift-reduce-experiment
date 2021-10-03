module Debugger exposing (debug1, debug2, debug3)

import Console


debug =
    True


debug1 label =
    if debug then
        Debug.log (Console.black (Console.bgMagenta (" " ++ label ++ " ")))

    else
        identity


debug2 label =
    if debug then
        Debug.log (Console.black (Console.bgCyan (" " ++ label ++ " ")))

    else
        identity


debug3 label =
    if debug then
        Debug.log (Console.black (Console.bgYellow (" " ++ label ++ " ")))

    else
        identity
