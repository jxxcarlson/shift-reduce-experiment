module Markup.Debugger exposing
    ( debug3
    , debugBlue
    , debugCyan
    , debugGreen
    , debugMagenta
    , debugNull
    , debugRed
    , debugYellow
    )

import Console


magentaSwitch =
    True


cyanSwitch =
    False


greenSwitch =
    True


yellowSwitch =
    True


blueSwitch =
    True


redSwitch =
    True


debugNull label =
    identity


debug3 label =
    identity --- Debug.log (Console.black (Console.bgYellow (" " ++ label ++ " ")))


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


debugGreen label =
    if greenSwitch then
         Debug.log (Console.black (Console.bgGreen (" " ++ label ++ " ")))

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
