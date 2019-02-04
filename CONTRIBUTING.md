## Contributing

### Requirements

#### Mac or Linux OS
I've only worked on this on a mac. I think it'll work on linux but not 100% sure.
For windows, I have no idea if it works but I haven't tried!

#### Haskell
Ideally be confident and proficient working in haskell. If you're new to haskell, please
don't be discouraged from contributing. Any PR I'm happy to look at and iterate on!

Also you need haskell and stack installed. Since we use stack, you don't have to worry
about which version of haskell because stack takes care of all of that.

#### LSP
Be a bit familiar how the language server protocol works.


### Deciding what to work on

I'm going to try to udpate the github issues with what good areas to contribute too. I'll try
to keep the issues specific as to what the end goal is. If you poke around and see something
that you think can be improved, feel free to work on that as well. Anything to improve this
codebase is a welcome change.

Additionally, I'll try to create and manage issues for what I'm working on as well, to avoid
doing duplicate work. If you have a question about if something already exists or a feature,
please open and issue and we can dicusss and find a path forward.

A few general notes on contributing.

- Try to avoid adding new dependencies unless it's really necessary
- The types in haskell do a good job doing simple self-documenting, however if something you're
working on is counter-intuitive then please add comments explaining why you're doing what you're
doing


Also, this a part-time project for me so if you open an issue or reach out it make take me a few days to get back to you.

## Layout of the project

This project loosely following a headless elm archiecture style pattern. Before starting, let's review the LSP protocol.
Briefly, it's a message passing system, with request messages which require a server response and a notification
message that the server does not need to respond. You can think of these messages as subscriptions in the elm
architecture.

To start off, we launch the [server](src/LSP/Server.hs) which is just a loop reading stdin. Then the client posts
a message in stdin for the server to read.

We have a [model](src/LSP/Model.hs), which outlines the state of the language server and is initialized before
we begin looping on stdin. Everytime we recieve a message from the client, we decode it and send it to the
[message handler](src/LSP/MessageHandler.hs). Here we optionally preform some side-effect based on the message 
and return a `Msg` that describes both changes in the model and what should be sent back to the client
(the latter functionality of the `Msg` may be reworked in the future). This message and the model are passed to the
[update function](src/LSP/Update.hs), which returns the new model and possibly some data to send back to the client.
Then we watch stdin with the new model waiting for the next message.

Generally, most features will involve adding in the message type to decode from the client, adding a handler for
that message that does some work and/or updates the model, then adding encoding for what the server should
respond to the client with.

Another thing to note is that each message handler function returns a [`Task`](src/LSP/Task.hs). A task is meant to
represent a side-effect that can either succeed or fail (it's just an `Either` wrapped in an `IO` that's a custom
type). This is used easily model side-effects with various errors and to use functions that we pull from the elm-compiler
easily. This type is the cornerstone of how this project manages side-effects.
