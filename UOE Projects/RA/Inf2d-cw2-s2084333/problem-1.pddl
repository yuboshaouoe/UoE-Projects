(define (problem warehouse-world)
    (:domain warehouse)
    (:objects 
        scanner - object
        mailbot - object
        package001 - object
        package010 - object
        package011 - object
        switch - switch
        belt - belt
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
        (Package package001)
        (Package package010)
        (Package package011)
        (MailBot mailbot)
        (Scanner scanner)
        (Switch switch)
        (DeliveryBelt belt)
        (On scanner c11)
        (On mailbot c33)
        (On package001 c15)
        (On package010 c35)
        (On package011 c55)
        (SwitchOn switch c53)
        (BeltOn belt c42)
        (BeltOn belt c52)
        (NextToBelt c32)
        (NextToBelt c41)
        (NextToBelt c43)
        (NextToBelt c51)
        (NextToBelt c53)
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
        (Connected c34 c44)
        (Connected c44 c34)
        (Connected c34 c35)
        (Connected c35 c34)
        (Connected c35 c45)
        (Connected c45 c35)
        (Connected c41 c51)
        (Connected c51 c41)
        (Connected c43 c53)
        (Connected c53 c43)
        (Connected c43 c44)
        (Connected c44 c43)
        (Connected c44 c54)
        (Connected c54 c44)
        (Connected c44 c45)
        (Connected c45 c44)
        (Connected c45 c55)
        (Connected c55 c45)
        (Connected c53 c54)
        (Connected c54 c53)
        (Connected c54 c55)
        (Connected c55 c54)
    )
    (:goal (and
        (Delivered package001)
    ))
)