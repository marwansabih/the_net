test2::IO()
test2 = do
    net <- generate_random_net 5 5 3 4
    print net
    putStrLn $ check_entries net
    print $ net_to_design $ net

test3::IO()
test3 = do
    net <- generate_random_net 5 5 3 4
    print $ net
    let design = net_to_design $ net
    print $ free_nodes design
    print $ full_nodes design
    putStrLn "Point_to (4,3):"
    print$ point_to (4,3) design
    putStrLn "Point_at (0,1):"
    print$ point_at (0,1) design
    putStrLn "Point_at (4,1):"
    print $ point_at (4,1) design

test4:: IO()
test4 = do
                net <- generate_random_net 5 5 3 2
                print net
                let design = net_to_design net
                let the_node = head $ full_nodes design
                print the_node
                print $ is_remove_able the_node design
                print "Pointing to"
                print $ point_to the_node design
                print "Pointing to at"
                let p_t = point_to the_node design
                print $ map(\x -> point_at x design) p_t
                print "Pointing at"
                print $ point_at the_node design
                print "Pointing at to"
                let p_at = point_at the_node design
                print $ map(\x -> point_to x design) p_at
                let bias_pos = last $ map (\(x,_) -> x) $ filter (\((a,_),_) -> a == 0 ) design
                print "Bias"
                print $ bias_pos
                print "All removeable nodes:"
                print $ remove_able_nodes design

test5:: IO()
test5 = do
        let design = [ ((0,0),[(0.1,(0,1))]), ((0,1),[(0.1,(0,1))]), ((0,2),[(0.1,(0,2)),(0.2,(0,1))]), ((1,1),[(0.3,(3,4))]),((1,2),[(0.3,(3,4))])]
        print $ design
        print $ remove_entry_at (0,2) (1,1) design
        print $ remove_neuron (1,1) design
        print $ remove_neuron (1,2) design

--testing the monad :)
test6 = do
    let design = [ ((0,0),[(0.1,(0,1))]), ((0,1),[(0.2,(0,4)),(0.1,(0,1))]), ((0,2),[(0.1,(0,2)),(0.2,(0,1))]), ((1,1),[(0.3,(3,4))]),((1,2),[(0.3,(3,4))])]
    let f = appD $ pure [(2,3),(4,5)] >>= add_f_connection (1,1) 0.5 >>= add_f_connection (1,1) 0.8
    print $ f design
    let g = appD $ pure [(0,1),(0,2)] >>= add_b_connection (1,1) 0.3 >>= add_b_connection (1,1) 0.7
    print $ g design
    let h = appD $ pure [(0,1),(0,2),(0,2)] >>= reset_b_weight (1,1) 11.0 >>= reset_b_weight (1,1) 17.0 >>= reset_b_weight (1,2) 18
    print $ h design
    let i = appD $ pure [(1,1),(1,1),(1,1)] >>= reset_f_weight (0,1) 11.0 >>= reset_f_weight (0,2) 17.0
    print $ i design
--testing more of the monad
test7 = do
    let design = [ ((0,0),[(0.1,(0,1))]), ((0,1),[(0.2,(0,4)),(0.1,(0,1))]), ((0,2),[(0.1,(0,2)),(0.2,(0,1))]), ((1,1),[(0.3,(3,4))]),((1,2),[(0.3,(3,4))])]
    g <- reset_f_weights (0,1) [(1,4),(1,1)]
    print $ (appD g) design
    h <- reset_b_weights (1,1) [(0,1),(0,2)]
    print $ (appD h) design
    n_des <- reset_weights (1,1) design
    print n_des

test8 = do
     drawn <- draw_from_list 3 (0,0) ([],[(9,0),(100,100),(3,3),(5,5),(1,1),(1,3)])
     print drawn
     let design = [ ((0,0),[(0.1,(0,1))]), ((0,1),[(0.2,(0,4)),(0.1,(0,1))]), ((0,2),[(0.1,(0,2)),(0.2,(0,1))]), ((1,1),[(0.3,(3,4))]),((1,2),[(0.3,(3,4))])]
     print $  d_b_nodes (1,1) design

test9 = do
    let design = [((0,0),[]),((0,1),[]),((0,2),[]),((0,3),[]),((0,4),[]),
                         ((1,0),[]), ((1,1),[]),((1,2),[]),((1,3),[]),((1,4),[]),
                         ((2,0),[]),((2,1),[]),((2,2),[]),((2,3),[]),((2,4),[]),
                         ((3,0),[]),((3,1),[]),((3,2),[]),((3,3),[]),((3,4),[])]
    d <- add_neuron_design 3 (1,1) design
    print d
    print "Testing to layers"
    let layers = to_layers 0 design []
    print layers
    print "Testing desing to net"
    print $ design_to_net d
    print "Testing altering neurons!"
    print "Will not be altered since no removeable neurons"
    (net1,net2) <- alter_neurons 1 3 (design_to_net d)
    print net1
    print net2
    let design2 = [((0,0),[(0.1,(0,0)),(0.2,(0,1))]),((0,1),[]),((0,2),[]),((0,3),[(0.1,(0,0)),(0.1,(0,1)),(0.1,(0,2))]),((0,4),[(0.1,(0,0)),(0.2,(0,1))]),
                         ((1,0),[(0.1,(1,0))]), ((1,1),[(0.1,(1,0))]),((1,2),[(0.1,(1,0))]),((1,3),[]),((1,4),[]),
                         ((2,0),[]),((2,1),[]),((2,2),[]),((2,3),[]),((2,4),[]),
                         ((3,0),[]),((3,1),[]),((3,2),[]),((3,3),[]),((3,4),[])]
    (net1',net2') <- alter_neurons 1 3 (design_to_net design2)
    print "should be altered"
    print net1'
    print net2'
    print "should be altered two ways"
    (net1'',net2'') <- alter_neurons 2 3 (design_to_net design2)
    print net1''
    print net2''




--finding the bug
test10 = do
                let design = [((0,0),[(6.577902026013427e-2,(0,3)),(2.9939257933782062e-2,(0,2)),(-2.6304549820658316e-2,(2,0)),(-8.218781819194819e-2,(0,0)),(-1.1041119102555008e-3,(2,1)),(9.166562213774526e-2,(3,2)),(9.05378837675605e-3,(3,1))]),
                                     ((0,1),[(9.575892321757908e-2,(0,0)),(3.3728207958921175e-2,(3,2)),(-3.3598829763540616e-3,(2,1)),(5.116839734361722e-2,(0,2)),(3.815340492950309e-2,(3,1)),(5.341642099381605e-2,(3,0))]),
                                     ((0,2),[(1.7587177824529055e-2,(2,1)),(1.0483077768658856e-2,(3,3)),(9.33338194596059e-3,(3,0)),(-8.180039627413525e-2,(0,2)),(-9.088420244961858e-2,(2,0)),(6.498912730986006e-2,(0,0)),(-8.039723440519693e-2,(0,3))]),
                                     ((0,3),[(2.06622430277605e-2,(0,0)),(7.07485417221132e-3,(3,2)),(-7.377633498164099e-2,(0,3)),(6.756810164273083e-2,(0,2)),(-2.5910111455625162e-2,(3,0))]),
                                     ((0,4),[(-1.0804173335471817e-2,(2,1)),(-4.534713681353983e-3,(0,3)),(-6.984759566290717e-2,(0,0)),(-2.703079250966789e-2,(0,2)),(-2.6008530633378588e-2,(2,0))]),
                                     ((1,0),[(-8.380828928374262e-2,(2,2)),(-4.9384994548781425e-2,(2,0)),(-7.229691541134933e-2,(2,3)),(-5.072614491225032e-2,(1,1)),(-3.741665170681188e-2,(1,0))]),
                                     ((1,1),[]),
                                     ((1,2),[(4.5506745229327045e-2,(1,0)),(-7.737301213380053e-2,(2,3)),(8.153423517419608e-2,(2,1)),(-5.690100118270332e-2,(2,0)),(-7.317350267921828e-2,(2,2))]),
                                     ((1,3),[(8.611906469786354e-2,(1,0)),(7.495540233583364e-2,(2,2)),(4.5880166508181414e-2,(2,0)),(-5.704446717367857e-2,(2,1)),(-2.358684659018792e-2,(1,1)),(-7.120445274711887e-2,(2,3))]),
                                     ((2,0),[]),
                                     ((2,1),[]),
                                     ((2,2),[]),
                                     ((2,3),[]),
                                     ((3,0),[(4.901577015119314e-2,(0,1)),(-2.2625291744335446e-2,(0,2)),(-5.525930785801241e-2,(0,3)),(5.023692217852105e-2,(0,0))]),
                                     ((3,1),[(5.906086079946843e-2,(0,0)),(-4.236237247904791e-2,(0,3)),(-7.71716743096109e-2,(0,1)),(6.96876629203621e-2,(0,2))]),
                                     ((3,2),[]),
                                     ((3,3),[]),
                                     ((4,0),[]),
                                     ((4,1),[]),
                                     ((4,2),[]),
                                     ((4,3),[])]
                print "Start"
                let net = design_to_net design
                (inp,outs) <- dat
                let inputs = cycle inp
                let outputs = cycle outs
                let n_net = train_data net (take 1500 inputs) (take 1500 outputs) 0.01
                print $ output n_net (inp !! 0)
                let design2 = [((0,0),[(2.4364809864687944e-2,(0,3)),(2.3725099428270974e-2,(3,1)),(-5.151568631749562e-2,(1,1)),(8.153692835933743e-3,(0,1)),(5.030914896643321e-2,(1,3)),(-4.04249290580107e-2,(3,0))]),
                                       ((0,1),[(1.5386864952611257e-2,(0,1)),(2.8862554480309433e-2,(1,2)),(7.86534117726621e-2,(3,0)),(-6.86080711099373e-2,(3,2)),(9.377925502003479e-2,(0,3)),(6.163764211733008e-2,(3,3)),(2.8098981637196513e-2,(1,1)),(2.9203984604679184e-3,(1,3))]),
                                       ((0,2),[(8.195226737896984e-2,(1,3)),(8.088159071021986e-2,(1,1)),(4.366182645563704e-2,(3,2)),(-9.394618813991656e-2,(0,3)),(2.284662010552782e-2,(0,1)),(-3.641795007464231e-2,(3,1)),(4.4983203446493925e-2,(1,2))]),
                                       ((0,3),[(5.121694585852429e-2,(3,1)),(6.301502019008023e-2,(0,1)),(6.547610596950526e-2,(1,3)),(8.934545258899534e-2,(3,3)),(-3.4100891660514554e-3,(0,3))]),
                                       ((0,4),[(-7.418398446884072e-2,(1,3)),(-9.646540963952593e-2,(1,1)),(-2.0787059838140443e-2,(0,3)),(-9.330217595077378e-3,(0,1)),(4.220460931136913e-2,(1,2))]),
                                       ((1,0),[]),
                                       ((1,1),[(9.904146922313506e-2,(2,1)),(9.218019437473024e-4,(2,2)),(-5.2319161621044045e-2,(2,3)),(-7.593263292926378e-2,(0,3)),(2.8092828586015917e-2,(2,0)),(5.0436775627131974e-2,(0,2)),(4.17503825892131e-2,(0,1))]),
                                       ((1,2),[]),
                                       ((1,3),[(2.676057324283404e-2,(0,3)),(2.952827987702547e-2,(2,3)),(-9.536372975897234e-2,(2,2)),(-8.056509860354877e-2,(0,1)),(1.6677107786702616e-2,(0,2))]),
                                       ((2,0),[]),
                                       ((2,1),[(5.932553263186535e-4,(1,3)),(6.435360548020883e-2,(1,1)),(7.780457439384353e-3,(1,0)),(4.871260461016658e-2,(1,2))]),
                                       ((2,2),[(1.0225066831412583e-2,(1,0)),(-9.161314098406358e-2,(1,1)),(6.899238281879269e-2,(1,2)),(-9.481184832392953e-2,(1,3))]),
                                       ((2,3),[(2.4708006669330262e-2,(1,2)),(-6.387611675385957e-3,(1,3)),(-5.433739951088173e-2,(1,1)),(4.339579798876608e-2,(1,0))]),
                                       ((3,0),[]),
                                       ((3,1),[]),
                                       ((3,2),[]),
                                       ((3,3),[]),
                                       ((4,0),[]),
                                       ((4,1),[]),
                                       ((4,2),[]),
                                       ((4,3),[])]
                let net2 = design_to_net design2
                let n_net2 = train_data net2 (take 1500 inputs) (take 1500 outputs) 0.01
                print $ output n_net2 (inp !! 0)
                let found_net = compete net net2 250   (take 10000 inputs, take 10000 outputs) 0.01
                print $ output (snd found_net) (inp !! 0)
                g_net <- generate_random_net 4 5 10 8
                print g_net
                a_net <- alter_neurons 1 8 g_net
                print $ a_net

test11 = do
    let width = 4
    let depth = 5
    let nr_neuron = 10
    let nr_con = 8
    let net = empty_net width depth
    print "creating empty_net worked"
    nodes <- generate_random_nodes net nr_neuron
    print "generating random nodes worked"
    design <- add_neighbours nodes nr_con
    print "add_neighbours to design worked"
    let gen_net = app_design net design
    print "transforming to net worked"
    net <- add_bias gen_net nodes
    print "adding bias worked"

test12 = do
    g_net <- generate_random_net 4 5 10 8
    print g_net
    let design = net_to_design g_net
    let f_ns = free_nodes design
    let d_ns = full_nodes design
    to_delete <- draw_uniform 1 d_ns
    to_create <- draw_uniform (length to_delete) f_ns
    d_design <- do_all to_delete design (to_IO remove_neuron)
    altered_design <- do_all to_create d_design (add_neuron_design 4)
    print "Done"

test13 = do
        net <-generate_random_net 4 20 10 5
        (inp,outs) <- dat
        (n_inp, n_out) <- get_random_batch 10 (take 40 inp, take 40 outs)
        --training_batch :: Net -> [[Double]] -> [[Double]] -> Double -> Net
        let t_net = training_batch net (n_inp) (n_out) 0.01
        print $ output t_net (inp !! 0)
        print  $ outs !! 0
        net1 <-generate_random_net 4 20 10 5
        net2 <-generate_random_net 4 20 10 5
        (err, b_net) <- compete_batch 2000 50 0.1 net1 net2 (inp,outs)
        print $ err
        print $ output  b_net (inp !! 0)
        print  $ outs !! 0

test14 = do
    (inp,outs) <- dat
    (n_inp, n_out) <- get_random_batch 10 (inp,outs)
    print (n_inp, n_out)

test15 = do
    net <-generate_random_net 4 20 10 5
    (inp,outs) <- dat
    let net' = get_gradient net (inp !! 0) (outs !! 0) 0.01
    let f  = map( \( _ ,ts) -> map ( \(w,_) -> w) ts)
    print $ f $ net_to_design net'
    print "orignal net"
    print $  f $  net_to_design net

test16 = do
    net  <- generate_random_net 5 5 3 3
    changed <- change_out_net 2 net
    print changed

test17 = do
    net <- generate_random_net 7 10 3  7
    changed <- change_out_net 1 net
    (a_net,n_net)<- alter_neurons 1 3 changed
    print a_net
    print n_net
    (inp,outs) <- dat
    --t_net <- training_batches 100 10 changed (inp,outs) 0.01
    --print "changed done"
    --b_net2 <- training_batches 100 10 a_net (inp,outs) 0.01
    --print "Done with a_net"
    --b_net2 <- training_batches 100 10 n_net (inp,outs) 0.01
    --print "Done with n_net"
    --update_random_net nr_times nr_trainings bs nr_alt_neuron nr_con sample net s
    found <- update_random_net 100 100 10 1 3 (inp,outs) changed 0.01
    print "Done wiht found"
    --(n_a, n_b) <- alter_neurons 1 3 a_net
    --print n_a
    --print n_b
    --(n_c, n_d) <- alter_neurons 1 3 n_net
    --print n_c
    --print n_d
