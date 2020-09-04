# Reasoning Engine (RE): Symbolic analysis of discrete dynamical systems

The Reasoning Engine is a proof-of-concept framework for SMT-based symbolic reasoning about discrete dynamical systems. It was inspired by the Reasoning Engine for Interaction Networks (RE:IN) project[<sup>1</sup>][Dunn2014]<sup>,</sup>[<sup>2</sup>][Yordanov2016], which applied SMT-based reasoning to biological interaction networks. RE:IN provided a domain-specific language (DSL) for encoding such networks together with experimental observations as an SMT problem and solving verification, synthesis and optimization problems using the Z3 prover[<sup>3</sup>][Z3]. To support the development of additional biological DSLs and support the analysis of other classes of biological models, a more flexible reasoning framework was required. The Reasoning Engine provides such a framework.


## Reasoning Engine Basics
The Reasoning Engine defines a set of data structures and a corresponding DSL (the Reasoning Engine Intermediate Language REIL) for describing generic discrete dynamical systems. It follows a Bounded Model Checking (BMC) approach to unroll system trajectories symbolically and allows for various constraints to be defined over these abstract executions of the system. The complete description of the system and associated constraints is then encoded as an SMT problem and solved using Z3, which enables various analysis, synthesis and optimization queries to be tackled. The intermediate language can be a compilation target for higher-level DSLs, thus accelerating the development of domain-specific reasoning tools. In the future, a client/server architecture could be developed where the Reasoning Engine exists as a service that could process REIL queries and return results to various client tools. 

## REIL Basics
A system is described by a number of discrete state variables, which could be of ``int`` (int), ``nat`` (non-negative integer) or ``bool`` (Boolean) type. Each variable can be either a ``system``, ``path`` or ``state`` variable. Path variables are replicated for each trajectory of the system that is considered as part of the analysis. State variables are replicated for each experiment and at every discrete time step and system variable are not replicated. Constraints are defined over the different variables of the system. Additional examples are available [here](Examples\ReasoningEngineBasics.ipynb).

[Dunn2014]: https://science.sciencemag.org/content/344/6188/1156.full
[Yordanov2016]: https://www.nature.com/articles/npjsba201610
[Z3]: https://github.com/Z3Prover/z3