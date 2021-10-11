
        IDENTIFICATION DIVISION.
            PROGRAM-ID. NeuralNetwork.
            AUTHOR. Gustavo Selbach Teixeira.
            DATE-WRITTEN. 2021-10-02.
            *> A simple feed forward neural network in Cobol
            *>   The logistical function can be configurable
            *>   using a "Leaky Relu" or "Sigmoid" function.
            DATA DIVISION.
                WORKING-STORAGE SECTION.
                    *> Set the parameters here. Please refer to README.md.
                    01 input_size           CONSTANT as 2.
                    01 hidden_size          CONSTANT as 4.
                    01 output_size          CONSTANT as 1.
                    01 n_hidden_layers      CONSTANT as 1.
                        *> "leaky_relu" or "sigmoid"
                    01 conf_log_function    CONSTANT as "leaky_relu".
                    01 conf_randomize_input CONSTANT as 0.
                    *> about the training
                    01 n_epochs             CONSTANT as 10000.
                    01 training_size        CONSTANT as 4.
                    01 learning_rate PIC S9V9(7) COMP-3 SYNC VALUE 0.1.
                    *> About the array sizes.
                        *> Probably dont need to touch this. Numbers are high.
                    01 n_layers             CONSTANT as 9.
                    01 weights_matrix_size  CONSTANT as 128.
                    *> End Parameters. All the rest your not supposed to touch.

                    01 nonlinear_function PIC X(20).
                    01 loss_function PIC X(20).
                    *> identifiers
                    01 input_layer INDEX SYNC VALUE 1.
                    01 output_layer INDEX SYNC VALUE 3.
                    *> useful vars
                    01 i INDEX SYNC VALUE 0.
                    01 j INDEX SYNC VALUE 0.
                    01 k INDEX SYNC VALUE 0.
                    01 n INDEX SYNC VALUE 0.
                    *> working vars
                    01 source_layer INDEX SYNC VALUE 0.
                    01 target_layer INDEX SYNC VALUE 0.
                    01 training_idx INDEX SYNC VALUE 0.
                    01 training_seq INDEX SYNC VALUE 0.
                    01 show_result  PIC 9V99 DISPLAY VALUE ZERO.
                    *> math
                    01 activation   PIC S9V9(7) COMP-3 SYNC.
                    01 errors       PIC S9V9(7) COMP-3 SYNC.
                    01 logistical   PIC S9V9(7) COMP-3 SYNC.
                    01 aux          PIC S9V9(7) COMP-3 SYNC.
                    01 aux2         PIC S9V9(7) COMP-3 SYNC.
                    *> date and seed
                    01 datetime     PIC X(21).
                    01 seed         PIC S9(7) BINARY.
                    *> training
                    01 training_data OCCURS training_size TIMES.
                        05 input_set OCCURS input_size TIMES.
                            10 inputs PIC S9V9(7) COMP-3 SYNC.
                        05 output_set OCCURS output_size TIMES.
                            10 outputs PIC S9V9(7) COMP-3 SYNC.
                    01 t_sequence OCCURS training_size TIMES.
                        05 training_sequence INDEX SYNC VALUE ZERO.
                    *> the layer object
                    01 layer_idx INDEX SYNC.
                    01 node_idx INDEX SYNC.
                    01 weights_idx INDEX SYNC.

                    01 layer OCCURS n_layers TIMES
                                INDEXED BY layer_idx.
                        05 n_connections PIC 999 COMP-3 VALUE ZERO.
                        05 n_nodes       PIC 999 COMP-3 VALUE ZERO.
                        05 nodes OCCURS weights_matrix_size TIMES
                                    INDEXED BY node_idx.
                            10 valuess  PIC S9V9(7) COMP-3 SYNC.
                            10 bias     PIC S9V9(7) COMP-3 SYNC.
                            10 deltas   PIC S9V9(7) COMP-3 SYNC.
                            10 synapses OCCURS weights_matrix_size TIMES
                                        INDEXED BY weights_idx.
                                15 weights PIC S9V9(7) COMP-3 SYNC.
 
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
                                    UNTIL training_seq > training_size
                        MOVE training_sequence(training_seq)
                                                TO training_idx
                        PERFORM set_intput
                        PERFORM pass_forward
                    END-PERFORM
                END-PERFORM.
            STOP RUN.
            
            *> Neural Network training process
            pass_forward.
                *> forward pass
                PERFORM VARYING source_layer FROM input_layer BY 1
                            UNTIL source_layer >= output_layer
                    ADD 1 TO source_layer GIVING target_layer
                    PERFORM activation_function
                END-PERFORM
                *> show results
                MOVE valuess(output_layer, 1) TO show_result.
                DISPLAY n " Input: ["valuess(input_layer, 1)
                        ", "valuess(input_layer, 2)
                        "] Expected: " outputs(training_idx, 1)
                        " Output: " show_result " - " valuess(output_layer, 1)
                        END-DISPLAY.
                *> back propagation
                PERFORM compute_loss.
                *> compute deltas and update weights
                MOVE output_layer TO source_layer.
                PERFORM VARYING source_layer FROM output_layer BY -1
                                    UNTIL target_layer <= input_layer
                    SUBTRACT 1 FROM source_layer GIVING target_layer
                    PERFORM calc_deltas
                    PERFORM update_weights
                END-PERFORM.
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
                    CALL nonlinear_function USING activation, logistical
                    MOVE logistical TO valuess(target_layer, j)
                END-PERFORM.
                EXIT.

            *> compute the delta for the output layer
            compute_loss.
                PERFORM VARYING i FROM 1 BY 1 UNTIL i > n_nodes(output_layer)
                    COMPUTE errors = (outputs(training_idx, i)
                                        - valuess(output_layer, i))
                    CALL loss_function USING valuess(output_layer, i),
                                             logistical
                    COMPUTE deltas(output_layer, i) = (errors * logistical)
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
                    CALL loss_function USING valuess(target_layer, j),
                                             logistical
                    COMPUTE deltas(target_layer, j) = (errors * logistical)
                END-PERFORM.
                EXIT.

            *> update connection's weights
            update_weights.
                PERFORM VARYING j FROM 1 BY 1 UNTIL j > n_nodes(source_layer)
                    COMPUTE bias(source_layer, j) = (bias(source_layer, j)
                                + (deltas(source_layer, j) * learning_rate))
                    PERFORM VARYING i FROM 1 BY 1 UNTIL i > n_nodes(target_layer)
                        COMPUTE weights(source_layer, i, j) = (
                            weights(source_layer, i, j)
                            + (valuess(target_layer, i)
                               * deltas(source_layer, j) * learning_rate))
                    END-PERFORM
                END-PERFORM.
                EXIT.

            *> Set network topology and randomize data
            initialize_network.
                *> set the network topology. n_nodes and connections of layers
                MOVE input_size TO n_nodes(input_layer).
                MOVE 0 TO n_connections(input_layer).
                *> set the multiple hidden layers, allowing for Deep NN.
                ADD 2 TO n_hidden_layers GIVING output_layer.
                MOVE input_layer TO source_layer
                PERFORM UNTIL target_layer >= output_layer
                    ADD 1 TO source_layer GIVING target_layer *> t = s + 1
                    MOVE hidden_size TO n_nodes(target_layer)
                    MOVE n_nodes(source_layer) TO n_connections(target_layer)
                    ADD 1 TO source_layer *> s++
                END-PERFORM.
                *> set the output_layer
                MOVE output_size TO n_nodes(output_layer).
                MOVE hidden_size TO n_connections(output_layer).
                *> seed random generator
                MOVE FUNCTION CURRENT-DATE TO datetime.
                MOVE datetime(8:9) TO seed.
                COMPUTE i = FUNCTION RANDOM(seed).
                *> initialize nodes with with randoms
                PERFORM VARYING i FROM 1 BY 1 UNTIL i > output_layer
                    PERFORM VARYING j FROM 1 BY 1 UNTIL j > n_nodes(i)
                        COMPUTE valuess(i, j) = FUNCTION RANDOM
                        COMPUTE bias(i, j) = FUNCTION RANDOM
                        COMPUTE deltas(i, j) = FUNCTION RANDOM
                        COMPUTE valuess(i, j) = FUNCTION RANDOM
                        PERFORM VARYING k FROM 1 BY 1 
                                        UNTIL k > n_connections(i)
                            COMPUTE weights(i, j, k) = FUNCTION RANDOM
                        END-PERFORM
                    END-PERFORM
                END-PERFORM.
                *> define logistical functions
                EVALUATE conf_log_function
                    WHEN "leaky_relu"
                        MOVE "leaky_relu" TO nonlinear_function
                        MOVE "d_leaky_relu" TO loss_function
                    WHEN "sigmoid"
                        MOVE "sigmoid" TO nonlinear_function
                        MOVE "d_sigmoid" TO loss_function
                END-EVALUATE
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

            *> randomly shuffles the array (slow)
            shuffle_array.
                PERFORM VARYING i FROM training_size BY -1 UNTIL i < 1
                    COMPUTE k = FUNCTION RANDOM * (i - 1 + 1) + 1
                    MOVE training_sequence(k) TO j
                    MOVE training_sequence(i) TO training_sequence(k)
                    MOVE j TO training_sequence(i)
                END-PERFORM.
        END PROGRAM NeuralNetwork.

        *> **************************************************
        *> Logistical functions
        *>
        *> Leaky Relu
        IDENTIFICATION DIVISION.
            PROGRAM-ID. leaky_relu.
            *> The Leaky ReLU function
            DATA DIVISION.
                LINKAGE SECTION.
                    01 x PIC S9V9(7) COMP-3 SYNC.
                    01 r PIC S9V9(7) COMP-3 SYNC.
            PROCEDURE DIVISION USING x, r.
                IF x > 0 THEN
                    MOVE x TO r
                ELSE
                    COMPUTE r = 0.01 * x
                END-IF.
            GOBACK.
        END PROGRAM leaky_relu.

        IDENTIFICATION DIVISION.
            PROGRAM-ID. d_leaky_relu.
            *> The derivative of Leaky ReLU
            DATA DIVISION.
                LINKAGE SECTION.
                    01 x PIC S9V9(7) COMP-3 SYNC.
                    01 r PIC S9V9(7) COMP-3 SYNC.
            PROCEDURE DIVISION USING x, r.
                IF x >= 0 THEN
                    MOVE 1 TO r
                ELSE
                    MOVE 0.01 TO r
                END-IF.
            GOBACK.
        END PROGRAM d_leaky_relu.

        *>**************************************************
        *> Sigmoid
        IDENTIFICATION DIVISION.
            PROGRAM-ID. sigmoid.
            *> The Sigmoid function
            DATA DIVISION.
                LINKAGE SECTION.
                    01 x PIC S9V9(7) COMP-3 SYNC.
                    01 r PIC S9V9(7) COMP-3 SYNC.
            PROCEDURE DIVISION USING x, r.
                COMPUTE r = 1 / (1 + FUNCTION EXP(-x))
            GOBACK RETURNING r.
        END PROGRAM sigmoid.

        IDENTIFICATION DIVISION.
            PROGRAM-ID. d_sigmoid.
            *> The derivative of Sigmoid
            DATA DIVISION.
                LINKAGE SECTION.
                    01 x PIC S9V9(7) COMP-3 SYNC.
                    01 r PIC S9V9(7) COMP-3 SYNC.
            PROCEDURE DIVISION USING x, r.
                COMPUTE r = x * (1 - x)
            GOBACK.
        END PROGRAM d_sigmoid.
