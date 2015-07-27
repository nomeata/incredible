This file specifies some fundamental data types of The Incredible Proof Machine.

Logic
-----

The logic object contains the information about the currently allowed inference
rules, i.e. our axiomatization. This is usually quite constant.

  * `logic`: Object with field `rules`

      * `rules`: A list of rule objects. This is a list, and not a key-value
        map, as the order might be intentional and should be preserved by the
        UI.

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
  "rules": [
    { "id": "conjI",
      "ports": {
        "in1": {"type": "assumption", "proposition": "A"},
        "in2": {"type": "assumption", "proposition": "B"},
        "out": {"type": "conclusion", "proposition": "A∧B"}
      }
    },
    { "id": "conjE",
      "ports": {
        "in":   {"type": "assumption", "proposition": "A∧B"},
        "out1": {"type": "conclusion", "proposition": "A"},
        "out2": {"type": "conclusion", "proposition": "B"}
      }
    },
    { "id": "impI",
      "ports": {
        "in":  {"type": "assumption",       "proposition": "B"},
        "hyp": {"type": "local hypothesis", "proposition": "A", "consumedBy": "in"},
        "out": {"type": "conclusion",       "proposition": "A → B"}
      }
    },
    { "id": "impE",
      "ports": {
        "in1": {"type": "assumption", "proposition": "A→B"},
        "in2": {"type": "assumption", "proposition": "A"},
        "out": {"type": "conclusion", "proposition": "B"}
      }
    }
  ]
}
```


Task
----

The task is the theorem to be proven, and thus tells the logic engine about the
assumptions availalbe, and the ultimate goal(s)

   * `task`: Object with fields `assumptions` and `conclusions`, which
     are lists of propositions, which are strings.


**Example**:

```JSON
task = {
    "assumptions": ["(A∧B)→C"],
    "conclusions": ["A→(B→C)"]
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

        Fields `from` and `to`, which contain port references:

          * A port reference object is either
               * an object with fields `block` and `port`, where where the
                 block fields contain the keys of the block object in the
                 proof, while the port fields contain the name of the port in
                 the corresponding rule.
               * an object with field `assumption`, which references a global
                 assumption. The value of the field is a number, starting with
                 1.
               * an object with field `conclusion`, which references a global
                 conclusion. The value of the field is a number, starting with
                 1.

**Example:**

```JSON
proof = {
  "blocks": {
    "b1": { "rule": "conjI"},
    "b2": { "rule": "impE"},
    "b3": { "rule": "impI"},
    "b4": { "rule": "impI"}
  },
  "connections": {
    "c1": {"from": {"assumption": 1},              "to": {"block": "b2", "port": "in1"}},
    "c2": {"from": {"block": "b2", "port": "out"}, "to": {"block": "b3", "port": "in"}},
    "c3": {"from": {"block": "b3", "port": "out"}, "to": {"block": "b4", "port": "in"}},
    "c4": {"from": {"block": "b4", "port": "out"}, "to": {"conclusion": 1}},
    "c5": {"from": {"block": "b4", "port": "hyp"}, "to": {"block": "b1", "port": "in1"}},
    "c6": {"from": {"block": "b3", "port": "hyp"}, "to": {"block": "b1", "port": "in2"}},
    "c7": {"from": {"block": "b1", "port": "out"}, "to": {"block": "b2", "port": "in2"}}
  }
}

```

Analysis
--------

This data structure describes everything the logic has to tell the UI about a given proof, within a given context.

  * `analysis`: Object with a bunch of fields:

      * `connectionPropositions`: Map from connection keys to propositions
        (strings), for those connections where a proposition could be inferred.
      * `unconnectedGoals`: A list of ports references (see above) of type
        assumption that need to be connected before the proof is complete.
      * `unificationFailures`: Map from connection key to
      * `cycles`: A list of cycles, where every cycle is a list of connection
        keys that form an illegal cycle.
      * `escapedHypotheses`: A list of paths, where every path is a list of connection
        keys that form a path from a local hypothesis to a conclusion of the task.
      * `qed`: Is `true` if the proof is complete.

        This implies that `unconnectedGoals` is empty.

        It does not necessarily imply that all of `unificationFailures`,
        `cycles` and `escapedHypotheses` are empty: It is possible, to have
        cycles and escapedHypotheses, as long as they are not used to prove any
        conclusions.

        Conversely, even if `unconnectedGoals` is empty, we can have `qed = false`
        if there are other problems.

**Example**

The analysis for the `context` above and the empty proof should be

```JSON
analysis = {
  "unconnectedGoals": {"conclusion": 1},
  "qed": false
}
```

With the proof given above, one would expect

```JSON
analysis = {
  "connectionPropositions": {
    "c1": "(A∧B)→C",
    "c2": "C",
    "c3": "B→C",
    "c4": "A→B→C",
    "c5": "A",
    "c6": "B",
    "c7": "A∧B",
  },
  "qed": true,
}
```


