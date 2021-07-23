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



## Run

After setted up; start the microservice with `make start-opt`, start the client service `npm run dev`,
and open `localhost:5000` in a browser.

## Setup

### Server

Install Idris2, its version can be found in the VERSIONS file.

For dependencies install `nodejs`, `npm` and packages:

```
npm install google-closure-compiler
npm install sqlite
npm install md5
```

To start the OrderTaking service, which will create the initial Sqlite DB and starts
the service on `127.0.0.1:3000`

```
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

Have fun!

## ASCII ART

[ASCII ART](https://dot-to-ascii.ggerganov.com/)
