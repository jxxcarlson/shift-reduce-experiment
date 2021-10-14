module Markup.Debugger exposing
    ( debugBlue
    , debugCyan
    , debugMagenta
    , debugRed
    , debugYellow
    )

import Console


magentaSwitch =
    False


cyanSwitch =
    False


yellowSwitch =
    True


blueSwitch =
    False


redSwitch =
    True


debugMagenta label =
    if magentaSwitch then
        Debug.log (Console.black (Console.bgMagenta (" " ++ label ++ " ")))

    else
        identity


debugCyan label =
    if cyanSwitch then
        Debug.log (Console.black (Console.bgCyan (" " ++ label ++ " ")))

    else
        identity


debugYellow label =
    if yellowSwitch then
        Debug.log (Console.black (Console.bgYellow (" " ++ label ++ " ")))

    else
        identity


debugBlue label =
    if blueSwitch then
        Debug.log (Console.white (Console.bgBlue (" " ++ label ++ " ")))

    else
        identity


debugRed label =
    if redSwitch then
        Debug.log (Console.black (Console.bgRed (" " ++ label ++ " ")))

    else
        identity
