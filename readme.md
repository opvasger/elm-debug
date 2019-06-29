# Elm DevTools
Tools for developing Elm programs!

![Demo Gif](/example/example.gif)

![NPM Version Badge](https://img.shields.io/npm/v/elm-devtools.svg)
![Elm-package Version Badge](https://img.shields.io/elm-package/v/opvasger/devtools.svg)

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

## Design
These devtools are based on the Elm architecture. Since programs are constructed using a few simple functions, they can be embedded and controlled, which also sets some limits for what these devtools can do.

### Automatic Message Replay
Any previous state of an application can be recomputed using the initial state and a list of messages folded by an update-function. These state-transitions happen independently of their environment - no commands are sent during replay, only the state changes. This is very useful to:
- Stop time to look at what happened.
- Go to a previous point in time to try something else.
- Reload without losing the state of your application.
- Reload with code changes but only partial (or no) loss of application state.

### Sessions
These devtools run in sessions. A session is essentially made up of:
1. devtools settings and state.
2. all messages that ever updated the debugged applications state.

Sessions keep track of what your doing during development. They persist through browser-reloads, code-changes, can be downloaded, sent, opened by collaborators, and submitted as bug-reports.

### Support for Code-Changes
To reliably support code-changes in sessions, it is essential that interactions are recorded rather than states. Interactions are modeled in Elm applications with a single type, usually called `Msg`. Let's do an example:
```elm
type Msg = Increment | Decrement

update msg count = case msg of
    Increment -> count + 1
    Decrement -> count - 1

state = List.foldl update 0 [ Increment, Decrement, Increment ]
```
If you run this example, the value of `state` will be `1`.

Refactor this into an application, and devtools would deal with changes to this code in different ways:
- If `Msg` was given another constructor, called `Reset` which updates `Reset -> 0`, the value of `state` is still `1`. Append-only changes are **always** compatible. Great.
- If `Msg` had `Decrement` removed (or changed), one of three strategies (of your choice) could be used:
    1. Restart the application, as `Decrement` has no meaning in the program anymore. The value of `state` is now `0`, and   we've accomplished nothing. This is how most web-app development works.
    2. Filter `Decrement` out of the list and update accordingly. The value of `state` is now `2`. This works better or worse depending on the coupling between the updates your messages perform. If you remove `LogIn` and expect `ViewAccount` to work, you will be dissapointed.
    3. Take messages until you reach the first `Decrement` and update using those. The value of `state` is now `1`. This is really great, as it captures the remaining valid sequence of updates the application could do. If `LogIn` goes away, you'll never try to `ViewAccount`.
- If `update` is changed, any sequence of messages will still be (generally) valid. `Increment` might do a `+ 2` instead, but messages still capture your interactions, so you can tweak `update` until it works as intended. You can tweak `view` and `subscriptions` in the same way.

By recording interactions rather than state, Elm applications can be written while running, and you will only ever have impared (but predictable) message-replay when you modify the definition of messages.