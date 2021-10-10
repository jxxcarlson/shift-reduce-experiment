module Markup.Debugger exposing (debug1, debug2, debug3, debug4)

import Console


debugMagenta =
    True


debugCyan =
    True


debugYellow =
    True


debugBlue =
    True


debug1 label =
    if debugMagenta then
        Debug.log (Console.black (Console.bgMagenta (" " ++ label ++ " ")))

    else
        identity


debug2 label =
    if debugCyan then
        Debug.log (Console.black (Console.bgCyan (" " ++ label ++ " ")))

    else
        identity


debug3 label =
    if debugYellow then
        Debug.log (Console.black (Console.bgYellow (" " ++ label ++ " ")))

    else
        identity


debug4 label =
    if debugBlue then
        Debug.log (Console.white (Console.bgBlue (" " ++ label ++ " ")))

    else
        identity
