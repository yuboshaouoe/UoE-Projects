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
        (On scanner c45)
        (On mailbot c42)
        (On package001 c13)
        (On package010 c21)
        (On package011 c11)
        (SwitchOn switch c52)
        (BeltOn belt c34)
        (BeltOn belt c35)
        (NextToBelt c33)
        (Connected c11 c12)
        (Connected c12 c11)
        (Connected c12 c22)
        (Connected c22 c12)
        (Connected c13 c14)
        (Connected c14 c13)
        (Connected c14 c15)
        (Connected c15 c14)
        (Connected c15 c25)
        (Connected c25 c15)
        (Connected c21 c31)
        (Connected c31 c21)
        (Connected c22 c32)
        (Connected c32 c22)
        (Connected c22 c23)
        (Connected c23 c22)
        (Connected c23 c24)
        (Connected c24 c23)
        (Connected c24 c25)
        (Connected c25 c24)
        (Connected c31 c41)
        (Connected c41 c31)
        (Connected c32 c42)
        (Connected c42 c32)
        (Connected c33 c43)
        (Connected c43 c33)
        (Connected c41 c51)
        (Connected c51 c41)
        (Connected c42 c43)
        (Connected c43 c42)
        (Connected c43 c44)
        (Connected c44 c43)
        (Connected c44 c45)
        (Connected c45 c44)
        (Connected c45 c55)
        (Connected c55 c45)
        (Connected c51 c52)
        (Connected c52 c51)
        (Connected c52 c53)
        (Connected c53 c52)
        (Connected c53 c54)
        (Connected c54 c53)
        (Connected c54 c55)
        (Connected c55 c54)
    )
    (:goal (and
        (Delivered package001)
        (Delivered package010)
        (Delivered package011)
    ))
)