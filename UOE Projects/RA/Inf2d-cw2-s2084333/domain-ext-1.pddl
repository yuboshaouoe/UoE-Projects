(define (domain warehouse-ext-1)
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
        (MailBot ?o - object)
        (Scanner ?o - object)
        (Package ?o - object)
        (DeliveryBelt ?b - belt)
        (Switch ?s - switch)
        (Station ?s - station)
    )
    
    (:functions
        (BatteryAmount ?m - object)
    )
       
    (:action MOVE
        :parameters (?m - object ?cell1 - cell ?cell2 - cell)
        :precondition (and (MailBot ?m) (On ?m ?cell1) (Connected ?cell1 ?cell2) (not (IsHolding ?m)) (>= (BatteryAmount ?m) 1))
        :effect (and (not (On ?m ?cell1)) (On ?m ?cell2) (decrease (BatteryAmount ?m) 1))
    )

    (:action RECHARGE
        :parameters (?m - object ?c - cell ?s - station)
        :precondition (and (MailBot ?m) (Station ?s) (On ?m ?c) (StationOn ?s ?c))
        :effect (and (assign (BatteryAmount ?m) 15))
    )
    
    (:action MOVEWITHOBJECT
        :parameters (?m - object ?cell1 - cell ?cell2 - cell)
        :precondition (and (MailBot ?m) (On ?m ?cell1) (Connected ?cell1 ?cell2) (IsHolding ?m) (>= (BatteryAmount ?m) 2))
        :effect (and (not (On ?m ?cell1)) (On ?m ?cell2) (decrease (BatteryAmount ?m) 2))
    )
    
    (:action PICKUP
        :parameters (?m - object ?o - object ?c - cell)
        :precondition (and (MailBot ?m) (On ?m ?c) (On ?o ?c) (not (IsHolding ?m)))
        :effect (and (not (On ?o ?c)) (IsHolding ?m) (ObjectHeld ?m ?o))
    )

    (:action DROP
        :parameters (?m - object ?o - object ?c - cell)
        :precondition (and (MailBot ?m) (On ?m ?c) (ObjectHeld ?m ?o))
        :effect (and (On ?o ?c) (not (ObjectHeld ?m ?o)) (not (IsHolding ?m)))
    )

    (:action DELIVER
        :parameters (?c - cell ?m - object ?p - object ?b - belt ?s - switch)
        :precondition (and (MailBot ?m) (Package ?p) (DeliveryBelt ?b) (ObjectHeld ?m ?p) (Scanned ?p) (On ?m ?c) (NextToBelt ?c) (TurnedOn ?s))
        :effect (and (not (ObjectHeld ?m ?p)) (not (IsHolding ?m)) (Delivered ?p))
    )

    (:action TURNONSWITCH
        :parameters (?m - object ?s - switch ?c - cell)
        :precondition (and (MailBot ?m) (Switch ?s) (On ?m ?c) (SwitchOn ?s ?c) (not (TurnedOn ?s)))
        :effect (and (TurnedOn ?s))
    )

    (:action SCAN
        :parameters (?m - object ?p - object ?k - object ?c - cell)
        :precondition (and (MailBot ?m) (Scanner ?k) (Package ?p) (ObjectHeld ?m ?k) (On ?m ?c) (On ?p ?c))
        :effect (and (Scanned ?p))
    )
)
