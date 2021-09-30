# A Neural Network in Cobol

A feed forward neural network in Cobol :)

Still kinda slow, but very accurate! I'm trying to make it run faster.

## usage:

If you just wanna try it.
```shell
    make run
```

### Change network parameters

In the DATA DIVISION there are the following vars:

```cobol
    01 n_epochs CONSTANT as 10000.
    01 input_size CONSTANT as 2.
    01 hidden_size CONSTANT as 6.
    01 output_size CONSTANT as 1.
    01 learning_rate USAGE IS COMP-2 VALUE 0.1.
```

- *n_epochs* How many loops the training will take
- *input_size* How many nodes at the Input Layer
- *hidden_size* How many nodes at the Hidden Layer
- *output_size* How many nodes at the Output Layer

### Change training data

In the paragraph named **insert_training_data**, add values to the tables *inputs* and *outputs*:

```cobol
    *> [0.0, 1.0]] [1.0]]
    MOVE 0 TO inputs(1, 1).
    MOVE 1 TO inputs(1, 2).
    MOVE 1 TO outputs(1, 1).
    *> [1.0, 1.0]] [0.0]]
    MOVE 1 TO inputs(2, 1).
    MOVE 1 TO inputs(2, 2).
    MOVE 0 TO outputs(2, 1).
```
