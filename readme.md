# Reasoning Engine (RE): Symbolic analysis of discrete dynamical systems

The Reasoning Engine is a proof-of-concept framework for SMT-based symbolic reasoning about discrete dynamical systems. It was inspired by the Reasoning Engine for Interaction Networks (RE:IN) project[<sup>1</sup>][Dunn2014]<sup>,</sup>[<sup>2</sup>][Yordanov2016], which applied SMT-based reasoning to biological interaction networks. RE:IN provided a domain-specific language (DSL) for encoding such networks together with experimental observations as an SMT problem and solving verification, synthesis and optimization problems using the Z3 prover[<sup>3</sup>][Z3]. To support the development of additional biological DSLs and support the analysis of other classes of biological models, a more flexible reasoning framework was required. The Reasoning Engine provides such a framework.


## Reasoning Engine Basics
The Reasoning Engine defines a set of data structures and a corresponding DSL (the Reasoning Engine Intermediate Language REIL) for describing generic discrete dynamical systems. It follows a Bounded Model Checking (BMC) approach to unroll system trajectories symbolically and allows for various constraints to be defined over these abstract executions of the system. The complete description of the system and associated constraints is then encoded as an SMT problem and solved using Z3, which enables various analysis, synthesis and optimization queries to be tackled. The intermediate language can be a compilation target for higher-level DSLs, thus accelerating the development of domain-specific reasoning tools. In the future, a client/server architecture could be developed where the Reasoning Engine exists as a service that could process REIL queries and return results to various client tools. 

## REIL Basics
A system is described by a number of discrete state variables, which could be of ``int`` (int), ``nat`` (non-negative integer) or ``bool`` (Boolean) type. Each variable can be either a ``system``, ``path`` or ``state`` variable. Path variables are replicated for each trajectory of the system that is considered as part of the analysis. State variables are replicated for each experiment and at every discrete time step and system variable are not replicated. Constraints are defined over the different variables of the system. Additional examples are available [here](Examples/ReasoningEngineBasics.ipynb).

## Examples
A number of example notebooks containing results from various case studies and published papers are made available as part of this project. So far, these include
* [ReasoningEngineBasics](Examples/ReasoningEngineBasics.ipynb): Illustration of the REIL language and Reasoning Engine infrastructure.
* [REIN](Examples/REIN.ipynb): Examples of using the RE:IN language to study stem cell decision making[<sup>1</sup>][Dunn2014].
* [YordanovDunnNSB2016](Examples/YordanovDunnNSB2016.ipynb): Additional exaples of using the RE:IN language to analyze various biological systems[<sup>2</sup>][Yordanov2016].
* [RESIN](Examples/RESIN.ipynb): Illustrates an extension of the RE:IN methodology that supports reasoning about switching interaction networks[<sup>3</sup>][Yordanov2016].
* [Motifs](Examples/Motifs.ipynb): Illustrates an extension of the RE:IN methodology that enables reasoning about function (dynamical behaviour) and structure (network motifs) of biological interaction networks[<sup>4</sup>][Motifs].

[Dunn2014]: https://science.sciencemag.org/content/344/6188/1156.full
[Yordanov2016]: https://www.nature.com/articles/npjsba201610
[Z3]: https://github.com/Z3Prover/z3
[RESIN]: https://www.sciencedirect.com/science/article/pii/S0303264716300338
[Motifs]: https://pubmed.ncbi.nlm.nih.gov/31722483/

## Tool installation
The Reasoning Engine tools are built using F# and there are install instructions available at https://fsharp.org/. Executing the provided [example Jupyter notebooks](Examples) requires either [IfSharp](https://github.com/fsprojects/IfSharp) or [.NET Interactive](https://github.com/dotnet/interactive). All examples are provided as IfSharp notebooks and several notebooks are also converted to .NET Interactive as a proof of concept (work in progress). Sample steps for setting up and executing the IfSharp notebooks locally are outlined below:
1. Download and install required tools, including
    1. [Git](https://git-scm.com/)
    1. [.NET Core](https://dotnet.microsoft.com/download/dotnet)
1. Clone the ReasoningEngine repo locally (e.g. `git clone https://github.com/fsprojects/ReasoningEngine.git`)
1. Follow the installation instructions on the [IfSharp project page](https://github.com/fsprojects/IfSharp) - this includes installing [Anaconda](https://www.anaconda.com/) (e.g. the version for Python 3.8)
1. Build the Reasoning Engine library and tools. In the top ReasoningEngine project folder execute the following:
    1. `dotnet tool restore`
    1. `dotnet build --configuration Release ./RENotebookAPI/RENotebookAPI.fsproj`
1. Start IfSharp in the `ReasoningEngine/Examples` folder using `jupyter notebook`
1. Open any of the IfSharp notebooks (excluding `ReasoningEngineBasicsDotNetInterative.ipynb` and `REINDotNetInteractive.ipynb`) and execute the cells in order