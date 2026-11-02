module Theme exposing (..)

import Element exposing (rgb255)


{-| Eldritch "Cthulhu" palette, <https://github.com/eldritch-theme/eldritch>
-}
eldritch =
    { sunkenDepths = rgb255 0x21 0x23 0x37
    , shallowDepths = rgb255 0x32 0x34 0x49
    , tidalSurface = rgb255 0x45 0x47 0x59
    , murkOverlay = rgb255 0x5B 0x5C 0x66
    , lighthouseWhite = rgb255 0xEB 0xFA 0xFA
    , oldOnePurple = rgb255 0x70 0x81 0xD0
    , wateryTombBlue = rgb255 0x04 0xD1 0xF9
    , greatOldOneGreen = rgb255 0x37 0xF4 0x99
    , dreamingOrange = rgb255 0xF7 0xC6 0x7F
    , pustulePink = rgb255 0xF2 0x65 0xB5
    , lovecraftPurple = rgb255 0xA4 0x8C 0xF2
    , rlyehRed = rgb255 0xF1 0x6C 0x75
    , goldOfYuggoth = rgb255 0xF1 0xFC 0x79
    }


colors =
    { bg = eldritch.sunkenDepths
    , surface = eldritch.shallowDepths
    , fg = eldritch.lighthouseWhite
    , inactive = eldritch.oldOnePurple
    , disabled = eldritch.murkOverlay
    , accent = eldritch.greatOldOneGreen
    }
