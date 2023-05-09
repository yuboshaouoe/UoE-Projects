(define (problem warehouse-world-ext-3)
    (:domain warehouse-ext-3)
    (:objects 
        scanner - object
        mailbot - object
        deliverybot - object
        package001 - object
        package010 - object
        package011 - object
        switch - switch
        station - station
        belt1 - belt
        belt2 - belt
        c11 - cell
        c12 - cell
        c13 - cell
        c14 - cell
        c15 - cell
        c21 - cell
        c22 - cell
        c23 - cell
        c24 - cell
        c25 - cell
        c31 - cell
        c32 - cell
        c33 - cell
        C34 - cell
        c35 - cell
        c41 - cell
        c42 - cell
        c43 - cell
        c44 - cell
        c45 - cell
        c51 - cell
        c52 - cell
        c53 - cell
        c54 - cell
        c55 - cell
    )
    
    (:init
        (= (MailBotBatteryAmount mailbot) 10)
        (= (DeliveryBotBatteryAmount deliverybot) 10)
        (= (BeltType belt1) 1)
        (= (BeltType belt2) 2)

        (Station station)

        (Package package001)
        (LargePackage package010)
        (LargePackage package011)
        (= (PackageType package001) 1)
        (= (LargePackageType package010) 2)
        (= (LargePackageType package011) 2)

        (MailBot mailbot)
        (DeliveryBot deliverybot)
        (Scanner scanner)
        (Switch switch)
        (DeliveryBelt belt1)
        (DeliveryBelt belt2)
        (On scanner c11)
        (On mailbot c33)
        (On deliverybot c51)
        (On package001 c15)
        (On package010 c35)
        (On package011 c55)
        (StationOn station c13)
        (SwitchOn switch c53)
        
        (BeltOn belt1 c42)
        (BeltOn belt1 c52)
        (NextToBelt1 c32)
        (NextToBelt1 c41)
        (NextToBelt1 c43)
        (NextToBelt1 c51)
        (NextToBelt1 c53)

        (BeltOn belt2 c54)
        (BeltOn belt2 c44) ;add belts next, remove connected, set package types
        (NextToBelt2 c34)
        (NextToBelt2 c43)
        (NextToBelt2 c45)
        (NextToBelt2 c53)
        (NextToBelt2 c55)

        (Connected c11 c12)
        (Connected c12 c11)
        (Connected c11 c21)
        (Connected c21 c11)
        (Connected c12 c22)
        (Connected c22 c12)
        (Connected c13 c23)
        (Connected c23 c13)
        (Connected c13 c14)
        (Connected c14 c13)
        (Connected c14 c24)
        (Connected c24 c14)
        (Connected c14 c15)
        (Connected c15 c14)
        (Connected c15 c25)
        (Connected c25 c15)
        (Connected c21 c22)
        (Connected c22 c21)
        (Connected c22 c32)
        (Connected c32 c22)
        (Connected c23 c33)
        (Connected c33 c23)
        (Connected c23 c24)
        (Connected c24 c23)
        (Connected c24 c34)
        (Connected c34 c24)
        (Connected c24 c25)
        (Connected c25 c24)
        (Connected c25 c35)
        (Connected c35 c25)
        (Connected c31 c41)
        (Connected c41 c31)
        (Connected c31 c32)
        (Connected c32 c31)
        (Connected c32 c33)
        (Connected c33 c32)
        (Connected c33 c43)
        (Connected c43 c33)
        (Connected c33 c34)
        (Connected c34 c33)
        ;(Connected c34 c44)
        ;(Connected c44 c34)
        (Connected c34 c35)
        (Connected c35 c34)
        (Connected c35 c45)
        (Connected c45 c35)
        (Connected c41 c51)
        (Connected c51 c41)
        (Connected c43 c53)
        (Connected c53 c43)
        ;(Connected c43 c44)
        ;(Connected c44 c43)
        (Connected c44 c54)
        (Connected c54 c44)
        ;(Connected c44 c45)
        ;(Connected c45 c44)
        (Connected c45 c55)
        (Connected c55 c45)
        ;(Connected c53 c54)
        ;(Connected c54 c53)
        ;(Connected c54 c55)
        ;(Connected c55 c54)
    )
    (:goal (and
        ;(TurnedOn switch)
        ;(On package011 c53)
        ;(Delivered package011)
        (Delivered package001)
        (Delivered package010)
    ))
)