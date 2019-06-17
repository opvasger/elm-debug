# Elm DevTools

![](/example/example.gif)

## Try out my example
The module is still being developed and will probably go through major changes.

If you want to try it, you can clone the repo and run these commands from the `/examples` folder:
```bash
# install dependencies
npm install

# start example with linux
npm start

# start example with windows
npm run start:win
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

### Supporting Code-changes
A major feature of these devtools is to be able to change code and have the application reload with new logic yet approximately the same state. I have a lot to say about what "approximately" means in this context:
1. If the code changes didn't involve the message-definition, you load into the state you would've been in if you took the exact same actions as before. It might look different because the view changed. It might have a different state because updates were performed differently. This is not a side-effect, but an important feature - **change code and you will 100% of the time see how those changes look in the state you were in before the changes**.
2. If you changed your message-definition, some part of it's previous definition will have lost any meaning in the program. These devtools have 3 built-in ways to deal with this:
    - Restart the application, which you generally don't want to do.
    - Go the state right before the first unkown message.
    - Skip over unknown messages and do all actions that are known in order. This basically works better or worse depending on the coupling between your messsages. If you remove your `LogIn` constructor and expect `ViewAccount` to work afterwards, you will be dissapointed.
