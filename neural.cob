
        IDENTIFICATION DIVISION.
            PROGRAM-ID. NeuralNetwork.
            AUTHOR. Gustavo Selbach Teixeira.
            DATE-WRITTEN. 2021-10-02.
            *> A simple feed forward neural network in Cobol
            *>   The logistical function can be configurable
            *>   using a "Leaky Relu" or "Sigmoid" function.
            DATA DIVISION.
                WORKING-STORAGE SECTION.
                    *> Logistical function definition - Dont touch this,
                    01 SIGMOID_FUN CONSTANT as 1.
                    01 RELU_FUN CONSTANT as 2.

                    *> Set the parameters here - You can touch here.
                    01 n_epochs CONSTANT as 10000.
                    01 input_size CONSTANT as 2.
                    01 hidden_size CONSTANT as 4.
                    01 output_size CONSTANT as 1.
                    01 learning_rate USAGE IS COMP-2 VALUE 0.1.
                    *> Set the logistical function: SIGMOID_FUN or RELU_FUN
                    01 conf_log_function CONSTANT as RELU_FUN.
                    *> Set if should randomize input.
                    *> Severely impacts performance, but improves training speed.
                    01 conf_randomize_input CONSTANT as 0.
                    *> End Parameters. All the rest your not supposed to touch.

                    *> identifiers CONSTANTS
                    01 input_layer USAGE IS INDEX VALUE 1.
                    01 hidden_layer USAGE IS INDEX VALUE 2.
                    01 output_layer USAGE IS INDEX VALUE 3.
                    *> useful vars
                    01 i USAGE IS INDEX VALUE 0.
                    01 j USAGE IS INDEX VALUE 0.
                    01 k USAGE IS INDEX VALUE 0.
                    01 n USAGE IS INDEX VALUE 0.
                    *> working vars
                    01 source_layer USAGE IS INDEX VALUE 0.
                    01 target_layer USAGE IS INDEX VALUE 0.
                    01 training_idx USAGE IS INDEX VALUE 0.
                    01 training_seq USAGE IS INDEX VALUE 0.
                    01 show_result  PIC 9V99 DISPLAY VALUE ZERO.
                    *> math
                    01 activation   USAGE IS COMP-2.
                    01 errors       USAGE IS COMP-2.
                    01 logistical   USAGE IS COMP-2.
                    01 aux          USAGE IS COMP-2.
                    01 aux2         USAGE IS COMP-2.
                    *> date and seed
                    01 datetime     PIC X(21).
                    01 seed         PIC S9(9) BINARY.
                    *> training
                    01 training_data OCCURS 4 TIMES.
                        05 input_set OCCURS 2 TIMES.
                            10 inputs USAGE IS COMP-2.
                        05 output_set OCCURS 1 TIMES.
                            10 outputs USAGE IS COMP-2.
                    01 t_sequence OCCURS 4 TIMES.
                        05 training_sequence USAGE IS INDEX VALUE ZERO.
                    *> the layer object
                    01 layer OCCURS 3 TIMES.
                        05 n_connections PIC 999 USAGE IS COMP VALUE ZERO.
                        05 n_nodes       PIC 999 USAGE IS COMP VALUE ZERO.
                        05 nodes OCCURS 16 TIMES.
                            10 valuess  USAGE IS COMP-2.
                            10 bias     USAGE IS COMP-2.
                            10 deltas   USAGE IS COMP-2.
                            10 synapses OCCURS 16 TIMES.
                                15 weights USAGE IS COMP-2.
 
            PROCEDURE DIVISION.
                PERFORM initialize_network.
                *> Do the NN main loop
                PERFORM VARYING n FROM 1 BY 1 UNTIL n > n_epochs
                    *> If true, randomize input array.
                    *> Improves accuracy but impacts performance.
                    IF conf_randomize_input = 1 THEN
                        PERFORM shuffle_array
                    END-IF
                    PERFORM VARYING training_seq FROM 1 BY 1
                                                UNTIL training_seq > 4
                        MOVE training_sequence(training_seq)
                                                TO training_idx
                        PERFORM set_intput
                        PERFORM pass_forward
                    END-PERFORM
                END-PERFORM.
            STOP RUN.
            
            *> Neural Network main procedure
            pass_forward.
                *> forward pass
                MOVE input_layer TO source_layer.
                MOVE hidden_layer TO target_layer.
                PERFORM activation_function.
                *> hidden to output
                MOVE hidden_layer TO source_layer.
                MOVE output_layer TO target_layer.
                PERFORM activation_function.
                MOVE valuess(output_layer, 1) TO show_result.
                DISPLAY n " Input: ["valuess(input_layer, 1)
                        ", "valuess(input_layer, 2)
                        "] Expected: " outputs(training_idx, 1)
                        " Output: " show_result " - " valuess(output_layer, 1)
                        END-DISPLAY.
                *> output delta
                PERFORM calc_output_delta.
                *> compute deltas
                MOVE output_layer TO source_layer.
                MOVE hidden_layer TO target_layer.
                PERFORM calc_deltas.
                *> update weights
                MOVE output_layer TO source_layer.
                MOVE hidden_layer TO target_layer.
                PERFORM update_weights.

                MOVE hidden_layer TO source_layer.
                MOVE input_layer TO target_layer.
                PERFORM update_weights.
                EXIT.
            
            *> set the input values for the training/prediction
            set_intput.
                PERFORM VARYING i FROM 1 BY 1 UNTIL i > n_nodes(input_layer)
                    MOVE inputs(training_idx, i) TO valuess(input_layer, i)
                END-PERFORM.
                EXIT.

            *> the activation function
            activation_function.
                PERFORM VARYING j FROM 1 BY 1 UNTIL j > n_nodes(target_layer)
                    MOVE bias(target_layer, j) TO activation
                    PERFORM VARYING i FROM 1 BY 1 
                                        UNTIL i > n_nodes(source_layer)
                        COMPUTE activation = activation + (
                                        valuess(source_layer, i) 
                                        * weights(target_layer, i, j))
                    END-PERFORM
                    *> logistical function. See: conf_log_function
                    EVALUATE conf_log_function
                        WHEN RELU_FUN
                            PERFORM relu_function
                        WHEN SIGMOID_FUN
                            PERFORM sigmoid_function
                    END-EVALUATE
                    MOVE logistical TO valuess(target_layer, j)
                END-PERFORM.
                EXIT.

            *> compute the delta for the output layer
            calc_output_delta.
                PERFORM VARYING i FROM 1 BY 1 UNTIL i > n_nodes(output_layer)
                    COMPUTE errors = (outputs(training_idx, i)
                                        - valuess(output_layer, i))
                    *> logistical function. See: conf_log_function
                    EVALUATE conf_log_function
                        WHEN RELU_FUN
                            MOVE errors TO deltas(output_layer, i)
                        WHEN SIGMOID_FUN
                            MOVE valuess(output_layer, i) TO aux
                            PERFORM d_sigmoid_function
                            COMPUTE deltas(output_layer, i) = errors * logistical
                    END-EVALUATE
                END-PERFORM.
                EXIT.
            
            *> compute deltas for the layers
            calc_deltas.
                PERFORM VARYING j FROM 1 BY 1 UNTIL j > n_nodes(target_layer)
                    MOVE 0 TO errors
                    PERFORM VARYING i FROM 1 BY 1 UNTIL i > n_nodes(source_layer)
                        COMPUTE errors = (errors + deltas(source_layer, i) 
                                          * weights(source_layer, j, i))
                    END-PERFORM
                    *> logistical function. See: conf_log_function
                    EVALUATE conf_log_function
                        WHEN RELU_FUN
                            COMPUTE deltas(target_layer, j) = (errors
                                            * valuess(target_layer, j))
                        WHEN SIGMOID_FUN
                            MOVE valuess(target_layer, j) TO aux
                            PERFORM d_sigmoid_function
                            COMPUTE deltas(target_layer, j) = (
                                            errors * logistical)
                    END-EVALUATE
                END-PERFORM.
                EXIT.

            *> update connection's weights
            update_weights.
                PERFORM VARYING j FROM 1 BY 1 UNTIL j > n_nodes(source_layer)
                    COMPUTE bias(source_layer, j) = (bias(source_layer, j)
                                + (deltas(source_layer, j) * learning_rate))
                    PERFORM VARYING i FROM 1 BY 1 UNTIL i > n_nodes(target_layer)
                        COMPUTE weights(source_layer, i, j) = (
                            (weights(source_layer, i, j)
                            + (valuess(target_layer, i)
                               * deltas(source_layer, j) * learning_rate)))
                    END-PERFORM
                END-PERFORM.
                EXIT.

            *> Set network topology and randomize data
            initialize_network.
                *> set the network topology
                MOVE input_size TO n_nodes(input_layer).
                MOVE 0 TO n_connections(input_layer).
                
                MOVE hidden_size TO n_nodes(hidden_layer).
                MOVE n_nodes(input_layer) TO n_connections(hidden_layer).
                
                MOVE output_size TO n_nodes(output_layer).
                MOVE n_nodes(hidden_layer) TO n_connections(output_layer).
                *> seed random generator
                MOVE FUNCTION CURRENT-DATE TO datetime.
                MOVE datetime(8:9) TO seed.
                COMPUTE i = FUNCTION RANDOM(seed).
                *> initialize nodes with with randoms
                PERFORM VARYING i FROM 1 BY 1 UNTIL i > 3
                    PERFORM VARYING j FROM 1 BY 1 UNTIL j > n_nodes(i)
                        COMPUTE valuess(i, j) = FUNCTION RANDOM
                        COMPUTE bias(i, j) = FUNCTION RANDOM
                        COMPUTE deltas(i, j) = FUNCTION RANDOM
                        MOVE FUNCTION RANDOM TO valuess(i, j)
                        PERFORM VARYING k FROM 1 BY 1 
                                        UNTIL k > n_connections(i)
                            MOVE FUNCTION RANDOM TO weights(i, j, k)
                        END-PERFORM
                    END-PERFORM
                END-PERFORM.
                *> training sequence
                PERFORM VARYING i FROM 1 BY 1 UNTIL i > 4
                    MOVE i TO training_sequence(i)
                END-PERFORM.
                PERFORM shuffle_array.
                PERFORM insert_training_data.
                EXIT.
            
            *> set the data for training
            insert_training_data.
                *> insert data to training sets.
                MOVE 0 TO inputs(1, 1).
                MOVE 0 TO inputs(1, 2).
                MOVE 0 TO outputs(1, 1).
                *> [1.0, 0.0], [1.0],
                MOVE 1 TO inputs(2, 1).
                MOVE 0 TO inputs(2, 2).
                MOVE 1 TO outputs(2, 1).
                *> [0.0, 1.0], [1.0],
                MOVE 0 TO inputs(3, 1).
                MOVE 1 TO inputs(3, 2).
                MOVE 1 TO outputs(3, 1).
                *> [1.0, 1.0]] [0.0]]
                MOVE 1 TO inputs(4, 1).
                MOVE 1 TO inputs(4, 2).
                MOVE 0 TO outputs(4, 1).
                EXIT.

            *> logistical function, takes "activation" as parameter
            sigmoid_function.
                COMPUTE logistical = 1 / (1 + FUNCTION EXP(-activation))
                EXIT.

            *> takes "aux" as parameter, returns "logistical"
            d_sigmoid_function.
                COMPUTE logistical = aux * (1 - aux)
                EXIT.

            *> the (leaky) Rectified Linear Unit function
            relu_function.
                IF activation > 0 THEN
                    MOVE activation TO logistical
                ELSE
                    *> This makes a Leaky Relu
                    COMPUTE logistical = 0.01 * activation
                    *> this is relu (not leaky)
                    *> MOVE 0 TO logistical
                END-IF.
                EXIT.

            *> The derivative of ReLU
            d_relu_function.
                IF aux >= 0 THEN
                    MOVE 1 TO logistical
                ELSE
                    MOVE 0 TO logistical
                END-IF.
                EXIT.

            *> randomly shuffles the array (slow)
            shuffle_array.
                PERFORM VARYING i FROM 4 BY -1 UNTIL i < 1
                    COMPUTE k = FUNCTION RANDOM * (i - 1 + 1) + 1
                    MOVE training_sequence(k) TO j
                    MOVE training_sequence(i) TO training_sequence(k)
                    MOVE j TO training_sequence(i)
                END-PERFORM.
        END PROGRAM NeuralNetwork.
