(define (domain warehouse-ext-2)
    (:requirements :adl :fluents)
    
    (:types cell object switch belt station)
    
    (:predicates 
        (Connected ?i - cell ?j - cell)
        (On ?o - object ?c - cell)
        (SwitchOn ?s - switch ?c - cell)
        (BeltOn ?b - belt ?c - cell)
        (StationOn ?s - station ?c - cell)
        (NextToBelt ?c - cell)
        (TurnedOn ?s - switch)
        (Scanned ?p - object)
        (IsHolding ?m - object)
        (ObjectHeld ?m - object ?o - object)
        (Delivered ?p - object)
        (MailBot ?m - object)
        (DeliveryBot ?d - object)
        (Scanner ?o - object)
        (Package ?o - object)
        (LargePackage ?o - object)
        (DeliveryBelt ?b - belt)
        (Switch ?s - switch)
        (Station ?s - station)
    )
    
    (:functions
        (MailBotBatteryAmount ?m - object)
        (DeliveryBotBatteryAmount ?d - object)
    )
       
    (:action MBMOVE
        :parameters (?m - object ?cell1 - cell ?cell2 - cell)
        :precondition (and (MailBot ?m) (On ?m ?cell1) (Connected ?cell1 ?cell2) (not (IsHolding ?m)) (>= (MailBotBatteryAmount ?m) 1))
        :effect (and (not (On ?m ?cell1)) (On ?m ?cell2) (decrease (MailBotBatteryAmount ?m) 1))
    ) 

    (:action DBMOVE
        :parameters (?d - object ?cell1 - cell ?cell2 - cell)
        :precondition (and (DeliveryBot ?d) (On ?d ?cell1) (Connected ?cell1 ?cell2) (not (IsHolding ?d)) (>= (DeliveryBotBatteryAmount ?d) 1))
        :effect (and (not (On ?d ?cell1)) (On ?d ?cell2) (decrease (DeliveryBotBatteryAmount ?d) 1))
    )

    (:action MBRECHARGE
        :parameters (?m - object ?c - cell ?s - station)
        :precondition (and (MailBot ?m) (Station ?s) (On ?m ?c) (StationOn ?s ?c))
        :effect (and (assign (MailBotBatteryAmount ?m) 15))
    )

    (:action DBRECHARGE
        :parameters (?d - object ?c - cell ?s - station)
        :precondition (and (DeliveryBot ?d) (Station ?s) (On ?d ?c) (StationOn ?s ?c))
        :effect (and (assign (DeliveryBotBatteryAmount ?d) 15))
    )
    
    (:action MBMOVEWITHOBJECT
        :parameters (?m - object ?cell1 - cell ?cell2 - cell ?o - object)
        :precondition (and (MailBot ?m) (not (LargePackage ?o)) (On ?m ?cell1) (Connected ?cell1 ?cell2) (ObjectHeld ?m ?o) (>= (MailBotBatteryAmount ?m) 2))
        :effect (and (not (On ?m ?cell1)) (On ?m ?cell2) (decrease (MailBotBatteryAmount ?m) 2))
    )

    (:action DBMOVEWITHOBJECT
        :parameters (?d - object ?cell1 - cell ?cell2 - cell ?o - object)
        :precondition (and (DeliveryBot ?d) (not (LargePackage ?o)) (On ?d ?cell1) (Connected ?cell1 ?cell2) (ObjectHeld ?d ?o) (>= (DeliveryBotBatteryAmount ?d) 2))
        :effect (and (not (On ?d ?cell1)) (On ?d ?cell2) (decrease (DeliveryBotBatteryAmount ?d) 2))
    )

    (:action MOVEOBJECTTOGETHER
        :parameters (?m - object ?d - object ?cell1 - cell ?cell2 - cell ?o - object)
        :precondition (and 
            (MailBot ?m) 
            (DeliveryBot ?d) 
            (LargePackage ?o) 
            (On ?m ?cell1) 
            (On ?d ?cell1) 
            (Connected ?cell1 ?cell2) 
            (ObjectHeld ?m ?o) 
            (ObjectHeld ?d ?o) 
            (>= (MailBotBatteryAmount ?m) 2)
            (>= (DeliveryBotBatteryAmount ?d) 2)
            )
        :effect (and 
            (not (On ?m ?cell1))
            (not (On ?d ?cell1))
            (On ?m ?cell2)
            (On ?d ?cell2)
            (decrease (MailBotBatteryAmount ?m) 2)
            (decrease (DeliveryBotBatteryAmount ?d) 2)
            )
    )
        
    (:action MBPICKUP
        :parameters (?m - object ?o - object ?c - cell)
        :precondition (and (MailBot ?m) (On ?m ?c) (On ?o ?c) (not (IsHolding ?m)) (Scanned ?o) (not (LargePackage ?o)) (not(MailBot ?o)) (not (DeliveryBot ?o)))
        :effect (and (not (On ?o ?c)) (IsHolding ?m) (ObjectHeld ?m ?o))
    )

    (:action MBPICKUPSCANNER
        :parameters (?m - object ?k - object ?c - cell)
        :precondition (and (MailBot ?m) (Scanner ?k) (On ?m ?c) (On ?k ?c) (not (IsHolding ?m)))
        :effect (and (not (On ?k ?c)) (IsHolding ?m) (ObjectHeld ?m ?k))
    )

    (:action DBPICKUP
        :parameters (?d - object ?o - object ?c - cell)
        :precondition (and (DeliveryBot ?d) (On ?d ?c) (On ?o ?c) (not (IsHolding ?d)) (Scanned ?o) (not (LargePackage ?o)) (not (MailBot ?o)) (not (DeliveryBot ?o)))
        :effect (and (not (On ?o ?c)) (IsHolding ?d) (ObjectHeld ?d ?o))
    )

    (:action DBPICKUPSCANNER
        :parameters (?d - object ?k - object ?c - cell)
        :precondition (and (DeliveryBot ?d) (Scanner ?k) (On ?d ?c) (On ?k ?c) (not (IsHolding ?d)))
        :effect (and (not (On ?k ?c)) (IsHolding ?d) (ObjectHeld ?d ?k))
    )

    (:action PICKUPTOGETHER
        :parameters (?m - object ?d - object ?o - object ?c - cell)
        :precondition (and 
            (MailBot ?m)
            (DeliveryBot ?d)
            (LargePackage ?o)
            (Scanned ?o)
            (On ?m ?c)
            (On ?d ?c)
            (On ?o ?c)
            (not (IsHolding ?m))
            (not (IsHolding ?d))
        )
        :effect (and 
            (not (On ?o ?c))
            (IsHolding ?m) 
            (IsHolding ?d) 
            (ObjectHeld ?m ?o)
            (ObjectHeld ?d ?o)
        )
    )

    (:action MBDROP
        :parameters (?m - object ?o - object ?c - cell)
        :precondition (and (MailBot ?m) (On ?m ?c) (ObjectHeld ?m ?o) (not (LargePackage ?o)))
        :effect (and (On ?o ?c) (not (ObjectHeld ?m ?o)) (not (IsHolding ?m)))
    )

    (:action DBDROP
        :parameters (?d - object ?o - object ?c - cell)
        :precondition (and (DeliveryBot ?d) (On ?d ?c) (ObjectHeld ?d ?o) (not (LargePackage ?o)))
        :effect (and (On ?o ?c) (not (ObjectHeld ?d ?o)) (not (IsHolding ?d)))
    )

    (:action DROPTOGETHER
        :parameters (?m - object ?d - object ?o - object ?c - cell)
        :precondition (and 
        (MailBot ?m)
        (DeliveryBot ?d)
        (LargePackage ?o)
        (On ?m ?c)
        (On ?d ?c)
        (ObjectHeld ?m ?o)
        (ObjectHeld ?d ?o)
        )
        :effect (and 
        (On ?o ?c)
        (not (ObjectHeld ?m ?o)) 
        (not (IsHolding ?m))
        (not (ObjectHeld ?d ?o)) 
        (not (IsHolding ?d))
        )
    )

    (:action DELIVERTOGETHER
        :parameters (?c - cell ?m - object ?d - object ?p - object ?b - belt ?s - switch)
        :precondition (and 
        (MailBot ?m)
        (DeliveryBot ?d)
        (LargePackage ?p)
        (DeliveryBelt ?b)
        (On ?m ?c)
        (On ?d ?c)
        (ObjectHeld ?m ?p)
        (ObjectHeld ?d ?p)
        (Scanned ?p)
        (NextToBelt ?c)
        (TurnedOn ?s)
        )
        :effect (and 
        (not (ObjectHeld ?m ?p)) 
        (not (IsHolding ?m))
        (not (ObjectHeld ?d ?p)) 
        (not (IsHolding ?d))
        (Delivered ?p)
        )
    )

    (:action MBDELIVER
        :parameters (?c - cell ?m - object ?p - object ?b - belt ?s - switch)
        :precondition (and (MailBot ?m) (Package ?p) (DeliveryBelt ?b) (ObjectHeld ?m ?p) (Scanned ?p) (On ?m ?c) (NextToBelt ?c) (TurnedOn ?s))
        :effect (and (not (ObjectHeld ?m ?p)) (not (IsHolding ?m)) (Delivered ?p))
    )

    (:action DBDELIVER
        :parameters (?c - cell ?d - object ?p - object ?b - belt ?s - switch)
        :precondition (and (DeliveryBot ?d) (Package ?p) (DeliveryBelt ?b) (ObjectHeld ?d ?p) (Scanned ?p) (On ?d ?c) (NextToBelt ?c) (TurnedOn ?s))
        :effect (and (not (ObjectHeld ?d ?p)) (not (IsHolding ?d)) (Delivered ?p))
    )
    
    (:action MBTURNONSWITCH
        :parameters (?m - object ?s - switch ?c - cell)
        :precondition (and (MailBot ?m) (Switch ?s) (On ?m ?c) (SwitchOn ?s ?c) (not (TurnedOn ?s)))
        :effect (and (TurnedOn ?s))
    )

    (:action DBTURNONSWITCH
        :parameters (?d - object ?s - switch ?c - cell)
        :precondition (and (DeliveryBot ?d) (Switch ?s) (On ?d ?c) (SwitchOn ?s ?c) (not (TurnedOn ?s)))
        :effect (and (TurnedOn ?s))
    )

    (:action SCANLARGEPACKAGE
        :parameters (?m - object ?p - object ?scanner - object ?c - cell)
        :precondition (and 
            (MailBot ?m) 
            (Scanner ?scanner) 
            (LargePackage ?p) 
            (ObjectHeld ?m ?scanner) 
            (On ?m ?c) 
            (On ?p ?c)
        )
        :effect (and (Scanned ?p))
    )

    (:action SCANPACKAGE
        :parameters (?m - object ?p - object ?scanner - object ?c - cell)
        :precondition (and (MailBot ?m) (Scanner ?scanner) (Package ?p) (ObjectHeld ?m ?scanner) (On ?m ?c) (On ?p ?c))
        :effect (and (Scanned ?p))
    )
)
