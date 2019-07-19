module UiFramework.Types exposing(..)

type Role
    = Primary
    | Secondary
    | Success
    | Info
    | Warning
    | Danger
    | Light
    | Dark


type ScreenSize
    = XS
    | SM
    | MD
    | LG
    | XL


getFontSize : ScreenSize -> Int
getFontSize size =
    case size of
        XS ->
            12
        
        SM ->
            14

        MD ->
            16

        LG ->
            20

        XL ->
            24