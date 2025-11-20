# Plutus Script Re-Executor

## Section 3: Problem Statements and Proposal Benefits

### Problem Statement

Developers of a decentralized application (dapp) would ideally have the option to easily analyze the successful executions of their scripts on the Cardano chain, if only to keep some off-chain state synchronized. To reduce load on the network, the fee formula incentivizes script authors to minimize the script’s logic and the size of the state it maintains. Unfortunately, this means state that does not influence the script’s validity judgment but is otherwise useful to the dapp developer (eg an individual user’s history of usage, statistics about general usage, etc) is not automatically available on-chain. Dapp developers must use auxiliary tooling in order to maintain that state off-chain such as the one proposed here.

This proposal would deliver a monitoring tool for that particular purpose that does not need to maintain its own full copy of the ledger state and so is significantly easier to deploy. Any tool that would otherwise need to maintain its own copy of the ledger state would benefit from the work described herein, but this initial proposal maintains a narrow focus on being able to re-execute a Plutus script with the same settings and arguments (aka the correct ScriptContext) as it was executed on-chain.

The Extended UTxO Model is a key foundation of the Plutus platform, but also fundamentally poses a specific challenge to the task of re-executing scripts: every UTxO that a transaction lists within its inputs is passed to every script required for that transaction’s validity. In particular, each script receives Plutus-representations of all the UTxOs lists as the transaction’s inputs, regardless of the script’s particular purpose (eg spending one such UTxO). As a consequence, dapp developers cannot anticipate whether some fresh UTxO will be relevant to future executions of their script—unless their script forbids the transaction from spending any UTxO beyond some ad-hoc predictable set, but that restriction would likely make their script burdensome and unpopular.

For this reason, access to the entire UTxO set is required in general to re-execute a script within the same settings and arguments as the Cardano node would execute the script. The node’s existing GetUTxOByTxIn local query suffices for accessing this data, but the node does not retain UTxO sets older than 2161 blocks—which usually arise in ~12 hours—so scripts could not necessarily be re-executed more than 12 hours later. For example, to execute a script as it was executed inside a block one week ago would require the UTxO set from one week ago, which a node could only provide access to if it were one week behind the network.

The only general option for dapp developers today is to maintain a second ledger state, using the local node as the source of blocks. However, running that separate process incurs costs of its own.

- The ledger state requires a significant amount of RAM. Satisfying both the node’s memory requirements as well as that of the monitor process may increase the developer’s infrastructure costs. This burden is significantly reduced by UTxO HD, but doubling the memory cost of the ledger state is likely to always be undesirable.

- With UTxO HD, the monitor process will similarly require gigabytes of additional disk space. Today the UTxO HD footprint is dwarfed by the node storing all of the blocks, but in the future the UTxO HD footprint itself might grow to a significant enough size that a second copy is undesirable.

- Etc.

### Proposal Benefit

This proposal would enable a more convenient experience for the Cardano dapp developer that needs to monitor the on chain execution of their scripts. In particular, this would enable off-chain analysis of on-chain script execution. The infrastructure would automatically maintain synchrony between the node and the monitor while minimizing redundant resource utilization.

In particular, the re-executor need not actually execute the same script. A major benefit is that it can instead execute an enrichment of the script that yields additional tracing information about the actual script’s internal dynamics. Such data does not truly belong on-chain and would increase fees if it were, but it can be very useful to dapp developers.

## Section 4: Proposal Details

### Proposal Name

Plutus Script Re-Executor

### Proposal Description

Dapp developers would use the proposed tool as follows.

- The dapp developer would run both a node following the Cardano network’s chain as well as the re-executor tool.

- The re-executor’s configuration data would include a map from script hash to the corresponding Plutus Core.

    - HOWEVER, this supplied Plutus Core can differ from the Plutus Core executed on-chain! In particular, it could be the result of compiling the same Plinth source code without providing the –remove-trace flag. In this way, the dapp developer could observe arbitrary tracing indicating their script’s actual on-chain execution dynamics without any Cardano node needing to waste computational costs on those traces or inflate the transaction’s fees accordingly.

- The dapp developer’s arbitrary downstream tooling will process the re-executor’s events.

    - The re-executor will emit an event each time a script identified in the configuration map is executed on-chain, including details about the corresponding re-execution. The event would also include the slot number, block number, and header hash.

    - The re-executor will also emit an event each time the local node updates its selection, including the slot number, block number, and header hash, which is sufficient information for the dapp developer to estimate of the on-going settlement probability for each script execution, according to standard Cardano settlement tables. (Kupo has some short and nice documentation about “Rollbacks & chain forks”, for context.)

    - The re-executor will additionally emit events identifying which script executions were undone whenever the local node switches away from the blocks that executed some scripts in the configuration data.

In order to provide those events to the dapp developers, the re-executor and local node will communicate as follows.

- The local node will push valid blocks to the re-executor via the Node-to-Client ChainSync mini protocol as it selects those blocks. Whenever the re-executor processes a block including IsValid=True transactions that executed Plutus scripts with hashes given in its configuration data, it would fetch the necessary data from the ledger state via queries such as GetUTxOByTxIn, execute the corresponding given Plutus Core with the same exact arguments (aka by rebuilding the correct ScriptContext) as the on-chain script execution, and emit an event that identifies each re-execution and includes its observed trace messages.

- In order to identify which transactions execute a script that matches one of the given hashes, the re-executor will either need to maintain some auxiliary indexes as it processes blocks to know which transaction inputs are guarded by a relevant script or— eg if those indexes grow too large—the re-executor can fallback on/be configured to instead use significant additional GetUTxOByTxIn queries instead.

- The local node will not discard a ledger state until the re-executor has signaled it no longer needs to query that ledger state.

    - Such coupling requires alterations to the node.

    - The MsgAcquire command within the LocalStateQuery mini protocol already provides this in some sense. However, if the re-executor starts falling further and further behind, this existing mechanism would simply increase the node’s memory usage without bound (disk space as well for UTxO HD).

    - Moreover, if the node restarts when the re-executor is retaining a historical ledger state, the re-executor would not be able to re-acquire that ledger state.

    - Designing the exact mechanism and ensuring it has a well-understood bounded detriment to the Cardano node and broader network as well as a tolerable experience for the dapp developer deploying it would be part of the first phase of the proposed work. One general extreme in this design space could rely on Ouroboros Genesis to follow the network’s chain during intervals where the re-executor (or whatever arbitrary tool is locally connected via this mechanism) was slower than the Cardano chain’s growth.


There are three key benefits to implementing this with the proposed architecture.

- It very clearly bounds how the node itself might be affected, which is important since that node is part of the Cardano network and moreover shares its source code with the rest of the network.

- It directly allows for a single node to serve multiple re-executors, even remotely if they have permission to tunnel in eg. To be clear: the slowest re-executor would determine whether that node can keep up with the Cardano chain’s growth.

- It does not assume that the node and the re-executor must always both be online, well-connected, healthy, and in sync — it is unrealistic to impose such a 100% up-time requirement on dapp developers. Instead, the architecture ensures that the local node will pause while the re-executor is unavailable.

### Dependencies

- It’s plausible that much of the re-executor’s logic could be reused from the cardano-ledger libraries if the Ledger Team is willing to include those parts within their stable interface.

- Similarly small requests might be submitted to the Plutus Team to ensure some functions useful to the re-executor are part of those libraries’ stable interface.

- At least the Consensus Team will need to review the proposed alternation to the node.

### Maintenance

- The re-executor would need an update in advance of hard forks, since each alters the codec for blocks and may alter the execution context for scripts.

- If the re-executor becomes popular, it would be worthwhile to parameterize how it emits script execution events: various EDA platform’s formats, etc.

- If this general architecture for monitoring becomes popular for other use cases beyond the re-executor, requests for new LocalStateTxQuery queries might become more frequent.

### Key Proposal Deliverables

- A design for the re-executor and its necessary interface to the node.

- A specification of the necessary node changes.

- An argument bounding how those changes might increase the node’s resource utilization or otherwise interfere with its primary responsibilities.

- An implementation of those changes in the node.

- An implementation of the re-executor itself.

- A robust test suite.

- Potentially also: requests to the Ledger Team and/or Plutus Team to tweak their libraries’ stable interface.


