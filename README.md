# Idris Full Stack DDD NodeJS example

## Order Taking Service

Idris version of Domain Modeling Made Functional Book.

 The [Domain Modeling Made Functional by Scott Wlaschin](https://www.amazon.co.uk/Domain-Modeling-Made-Functional-Domain-Driven/dp/1680502549)
introduces domain driven design and shows how well it fits to the world of functional programming via the lens of
the F# programming language. As Scott showed us that DDD is indeed a nice fit for functional programming,
questions arise naturally; could we push further the abstractions if we use a dependently typed programming language, like Idris?
Where are the sweet spots of depedent types in the world of everyday programming?
 Everyday programming needs to be focused on delivery, maintanability, reliability, and being correct.
The sad truth is that many applications needs to be more maintanable than correct.
Being correct is not a clear concept because its meaning changes as the software evolves.
In many cases, stakeholders only get a deeper understanding of the domain as the software solution/product evolves over time.
In this setting, depedent typed programming helps us achieve maintanability rather than being correct.
 In this repository I show a simple layered architecture and I show how to use simple dependent types
to draw explicit connections between the high level design of a service and its NodeJS deployed implementation.
Ideas and practices originated, but reshuffled from the [Type Driven Development with Idris by Edwin Brady](https://www.amazon.co.uk/Type-driven-Development-Idris-Edwin-Brady/dp/1617293024)
and the [Domain Modeling Made Functional by Scott Wlaschin](https://www.amazon.co.uk/Domain-Modeling-Made-Functional-Domain-Driven/dp/1680502549).
This architecture includes; An abstraction to talk about Bounded Context and Workflow, type safe description of a state transition system,
a free monad approach for domain implementation, and an actual implementation of the domain on NodeJS back-end.
 Because of dependent types, this architecture becomes explicit, rather than implicit, meaning that
connections between the high level design and the low level implementation are done via functions, changing
the code anywhere requires to think at the whole, as possible type errors propagates to top or to bottom.
 Idris could immensely benefit from simple FFIs for NodeJS libraries. The FFIs would grant access to thousands
of libraries from the NodeJS ecosystem almost for free. This approach would position Idris to be used even
in production settings and the Idris userbase could be bootstrapped, later the Idris version of these
libraries could be written.

Request: Please consider buying both of the books mentioned above. Boths are excellent resources for
further studying of the applied subject.

## Work In Progress parts:

- The documentation is still under development. Please come back regurarly to see what changed.
- Client needs lots of improvements.
- Better error Handling around workflow runners.
- Dependently driven testing needs to be implemented.

## Financial Support

<a href="https://www.patreon.com/AndOrP">
<img src="https://c5.patreon.com/external/logo/become_a_patron_button.png" width="150"/>
</a>

## Talk

<a href="https://youtu.be/QBj-4K-l-sg">Youtube</a>

## Notes

This project meant to be a blue-print, example for micro-services written in Idris. Its main focus is to reproduce
abstractions in dependently typed setting, that can be found in Scott Wlashin's book. The main focus is
around the dependently typed implementation of `BoundedContext`, but several other parts of the architecture
had to be worked on to be able to demostrate that Idris can host such solutions. Although during the implementation
many other problems were required to think about in the dependently typed setting. This repository can be
considered as a mine practices for dependently typed software development. Enjoy digging up those gems.

You may find the style of the explanations strange, maybe a bit chaotic, but if you follow path suggested
in the [READMAP.md](https://github.com/andorp/order-taking/blob/main/READMAP.md) you will learn many things about the Idris language. This style feels chaotic that could
be because I have a slight ADHD and I try to linarize the content here, but sometimes quickly the explanations
go deep. Although I believe a short explanation after every Idris snippet helps to view this reposity as
a hand-book for Idris development.

## High level overview

This repository contains more than just the `Idris` one to one translation of the `F#` code from the
Domain Modeling Made Functional book. I wanted to show how Idris can be used to interface with existing
`NodeJS` libraries. For that reason I added the following parts:

- FFI for NodeJS interfacing libraries
- Sketch of type-safe SQL library
- Minimal scaffolding of a NodeJS server
- Dependently typed framework for Bound-Context and Workflows
- State transition of the PlaceOrder example
- Free monadic DSL formalization of the PlaceOrder example
- One backend implementation of the operations of PlaceOrder DSL

See the [slides](https://github.com/andorp/order-taking/blob/main/SLIDES.md)

## Run

After setted up; start the microservice with `make start-opt`, start the client service `npm run dev`,
and open `localhost:5000` in a browser.

## Setup

### Server

 * Install Idris2 which version must match the one marketd the [VERSIONS](https://github.com/andorp/order-taking/blob/main/VERSIONS) file. OR
 * If you Idris2 release version installed, check out the corresponding tag from 0.4.0

For dependencies install `nodejs`, `npm` and packages:

```
npm install google-closure-compiler
npm install sqlite
npm install md5
```

To start the OrderTaking service, which will create the initial Sqlite DB and starts
the service on `127.0.0.1:3000`

```
make init-db
make start-opt
```

### Client

An example client can be found in the `client/svelte-client` directory. Follow the instructions
from its README.md

To run the client, call the following command:

```
cd client/svelte-client
npm run dev
```

Which starts the client in development mode, any changes in its code, will be picked up.

Important note, use the `g21` as product code on the client side for testing. The client
is very in pre-alpha state, best to open the developer console to see, what happens before and
after submitting the order. I will work on these details soon.

Have fun!

## ASCII ART

[ASCII ART](https://dot-to-ascii.ggerganov.com/)
