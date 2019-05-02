# Elm DevTools
Tools for developing Elm programs!

![](/example/example.gif)

## Try out my example
The module is still being developed and will probably go through major changes.

If you want to try it, you can clone the repo and run these commands from the root folder:
```bash
# install dependencies
npm install

# start example with mac/linux
npm run example

# start example with windows
npm run example:win
```

## Goals
The overarching goal is to close the loop between writing Elm code and interacting with it. This concretely means:
1. Code changes should be reflected immediately in a running instance of its application.
2. Code changes should only break application-state if necessary, and if enabled, only partially.
3. Application-states should be effortless to inspect, navigate, persist and share.
4. Guidance from the compiler should be right in front of you when you make mistakes.
5. The boilerplate necessary to enable the first four goals should be minimal, and if needed be, incremental.

I strongly believe that the optimal design and implementation of these goals will transform how we build our applications. Going through the loop of compiling code, reloading browser windows and getting the application into the right state costs seconds, but those seconds are spent at such a frequency that writing interactive applications is incredibly time-consuming. I hope it doesn't have to be.