# Meditation Bell

A simple meditation timer that rings a bell — at the start, at the end, and, if you like, at random moments in between.

**Use it:** [meditation.pegov.dev](https://meditation.pegov.dev)

## Features

- **Meditation bell.** Set a session from 5 to 60 minutes and press play. A bell marks the beginning and a longer one marks the end.
- **Warm-up.** An optional settling-in countdown (0:30 – 5:00) before the session starts, so you can get comfortable first.
- **Random interval bells.** Optional soft bells at unpredictable moments during the session: a gentle reminder to come back to the breath, placed so that the mind can't anticipate the next one. Longer sessions get more of them, never closer than a minute apart.
- **Keep the screen on, or don't.** Choose whether the screen stays on during a session. With it off you can lock the phone and save battery; the bells still ring.
- **Installable.** Add it to your phone's home screen and it opens full screen like a native app.
- **Works offline.** Once opened, everything — including the bell sounds — is cached on the device, so it works without a connection.

## Install on your phone

- **iPhone (Safari):** open the site, tap **Share**, then **Add to Home Screen**.
- **Android (Chrome):** open the site, tap the **⋮** menu, then **Add to Home screen** (or **Install app**).

## Development

Written in [Elm](https://elm-lang.org) 0.19 with [elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/). Uses [Task](https://taskfile.dev) to run the build commands.

```sh
task dev     # dev server with live reload and the Elm debugger
task build   # production build into docs/
task serve   # build and serve on the LAN, to test installing on a phone
task icons   # render the PNG icons from public/icons/icon.svg
```

The site is served by GitHub Pages from `docs/`. Run `task build` and commit `docs/` to deploy.
