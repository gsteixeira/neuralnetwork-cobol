# A Neural Network in Cobol

A feed forward neural network in Cobol :)

With configurable logistical function, you can use *sigmoid* or *leaky relu*.

By using relu, and randomizing input, a 100% accuracy can be achieved by as few as 500 epochs!

## usage:

If you just wanna try it.
```shell
    make run
```

Compile and run.
```shell
    make
    ./neuralnet
```

### Set the network parameters

In the DATA DIVISION there are the following vars:

```cobol
    01 n_epochs CONSTANT as 10000.
    01 input_size CONSTANT as 2.
    01 hidden_size CONSTANT as 6.
    01 output_size CONSTANT as 1.
    01 learning_rate USAGE IS COMP-2 VALUE 0.1.
    *> Set the logistical function: SIGMOID_FUN or RELU_FUN
    01 conf_log_function CONSTANT as RELU_FUN.
    *> Set if should randomize input.
    *> Severely impacts performance, but improves training speed.
    01 conf_randomize_input CONSTANT as 0.
```

- *n_epochs* How many loops the training will take
- *input_size* How many nodes at the Input Layer
- *hidden_size* How many nodes at the Hidden Layer
- *output_size* How many nodes at the Output Layer
- *conf_log_function* The logistical function. Can be: RELU_FUN or SIGMOID_FUN.
- *conf_randomize_input* If should randomize input. Severely impacts performance but improves training speed.

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

## Note on logistical functions

The first version of this used *sigmoid* as logistical function. However this had a very poor performance.
I found that the cullprint was the intrinsic function REM. Disabling it improves execution time by 1 minute!

So I refactored to use ReLU, but Leaky ReLU is even better. So now you can configure the constant **conf_log_function** to define which logistical function fits better your needs.

## To Be Done

I'm planning to make this a reusable library that can be used by other Cobol programs. So, stay tuned.

## For simplistic examples

When you are learning, the simple, the better. The main program here became too complex because of added features and optimizations to be didatic. So I'll keep simple examples at the *examples/* directory.

Look at there for examples more *easier to read* (Ha.. another Cobol bad joke!).

## I'm new to Cobol. How do I compile this?

You need to install the **gnucobol** package. The Cobol compiler is called **cobc**.

For debian variants:

```shell
    apt-get install gnucobol
```

For redhat variants:

```shell
    yum install gnucobol
```

Then compile using **cobc** like this:

```shell
    cobc -xj --free source_file.cob
```



