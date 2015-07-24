This file specifies some fundamental data types of The Incredible Proof Machine.

Context
-------

The context contains the information about the current task (or theorem to be
proven), i.e. everything that does not change during the user's interaction.


  * `context`: Object with fields `proposition` and `rules`

      * `proposition`: Object with fields `assumtions` and `conclusions`, which are lists of
        propositions, which are strings.

      * `rules`: A list of rule objects. Why a List?

          * rule objects:

            This object describes one rule of our logic, i.e. the basic building
            block on the graph.

            One field, `id` gives the (internal) name of the rule, which will be
            referenced by other objects.

            The field, `ports`, is a map from port names (strings) to port objects.

            The UI will likely want further fields here, e.g. the image to be used.

              * port object.

                This describes the bit of the rule that can be connted to other rules.
                Fields:
                  * `type`:
                    One of `assumption` or `conclusion` or `local hypothesis`.
                  * `consumedBy`:
                    Only if `type == local hypothesis`: Name of the port of this rule
                    object which needs to consume this local hypothesis.
                    (Probably not relevant for the UI).
                  * `proposition`:
                    The proposition assumed or provided by this port.

**Example**:

```JSON
context = {
  'proposition': {
    'assumptions': ['(A∧B)→C'],
    'conclusions': ['A→(B→C)']
  },
  'rules': [
    { 'id': 'conjI',
      'ports': {
        'in1': {'type': 'assumption', 'proposition': 'A'},
        'in2': {'type': 'assumption', 'proposition': 'B'},
        'out': {'type': 'conclusion', 'proposition': 'A∧B'},
      },
    },
    { 'id': 'conjE',
      'ports': {
        'in':   {'type': 'assumption', 'proposition': 'A∧B'},
        'out1': {'type': 'conclusion', 'proposition': 'A'},
        'out2': {'type': 'conclusion', 'proposition': 'B'},
      },
    },
    { 'id': 'impI',
      'ports': {
        'in':  {'type': 'assumption',       'proposition': 'B'},
        'hyp': {'type': 'local hypothesis', 'proposition': 'A', consumedBy: 'in'},
        'out': {'type': 'conclusion',       'proposition': 'A → B'},
      },
    },
    { 'id': 'impE',
      'ports': {
        'in1': {'type': 'assumption', 'proposition': 'A→B'},
        'in2': {'type': 'assumption', 'proposition': 'A'},
        'out': {'type': 'conclusion', 'proposition': 'B'},
      },
    },
  ]
}
```



Proof
-----

A proof is a graph consising of blocks for assumtions, conclusions and rules,
as well as connectors.

  * `proof`: Object with fields `blocks` and `connections`. Both are maps from
    keys (chosen by the UI) to objects.
      * block object: Object with a `rule` field (and probably further fields of private use for the UI).
          * `rule`: Id of the rule object used.
      * connection object:

        Fields `fromBlock`, `toBlock`, `fromPort`, `toPort`,
        where the block fields contain the keys of the block object in the proof, while the port fields contain the name of the port in the corresponding rule.

        A special block name is `proposition`, which refers to the proposition
        in the context, and has implicit ports `conclusion`*n* and
        `assumption`*n*, where `conclusion1` is the first conclusion etc.

**Example:**

```JSON
proof = {
  blocks = {
    'b1': { 'rule': 'conjI'},
    'b2': { 'rule': 'impE'},
    'b3': { 'rule': 'impI'},
    'b4': { 'rule': 'impI'},
  },
  connections = {
    'c1': {'fromBlock': 'proposition', 'fromPort': 'assumption1', toBlock: 'b2', toPort: 'in1'},
    'c2': {'fromBlock': 'b2', 'fromPort': 'out', toBlock: 'b3', toPort: 'in'},
    'c3': {'fromBlock': 'b3', 'fromPort': 'out', toBlock: 'b4', toPort: 'in'},
    'c4': {'fromBlock': 'b4', 'fromPort': 'out', toBlock: 'proposition', toPort: 'conclusion1'},
    'c5': {'fromBlock': 'b4', 'fromPort': 'hyp', toBlock: 'b1', toPort: 'in1'},
    'c6': {'fromBlock': 'b3', 'fromPort': 'hyp', toBlock: 'b1', toPort: 'in2'},
    'c7': {'fromBlock': 'b1', 'fromPort': 'out', toBlock: 'b2', toPort: 'in2'},
  }
}

```

Analysis
--------

This data structure describes everything the logic has to tell the UI about a given proof, within a given context.

  * `analysis`: Object with a bunch of fields:

      * `connectionPropositions`: Map from connection keys to propositions
        (strings), for those connections where a proposition could be inferred.
      * `unsolvedGoals`: A list of ports of type assumption that need to be connected before
        the proof is complete. Every port is specified by an object with fields
        `block` and `port`. Again, `block` can be `proposition`, then `goal`
        can be `conclusion`*n*.
      * `unificationFailures`: Map from connection key to
      * `cycles`: A list of cycles, where every cycle is a list of connection
        keys that form an illegal cycle.
      * `escapedHypotheses`: A list of paths, where every path is a list of connection
        keys that form a path from a local hypothesis to a conclusion of the proposition.
      * `qed`: Is `true` if the proof is complete. This should be equiavalent to
        `unsolvedGoals`, `unificationFailures`, `cycles` and
        `escapedHypotheses` being absent or empty.

**Example**

The analysis for the `context` above and the empty proof should be

```JSON
analysis = {
  'unsolvedGoals': {'block': 'proposition', 'port': 'conclusion1'}
}
```

With the proof given above, one would expect

```JSON
analysis = {
  'connectionPropositions': {
    'c1': '(A∧B)→C',,
    'c2': 'C',
    'c3': 'B→C',
    'c4': 'A→B→C',
    'c5': 'A',
    'c6': 'B',
    'c7': 'A∧B',
  },
  'qed': true,
}
```


