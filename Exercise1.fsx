type Cargo = A | B
type Time = int

type CargoState = {
    factoryCargo: Cargo list
    portDeliveries: (Cargo * Time) list
    aDeliveries: (Cargo * Time) list
    bDeliveries: (Cargo * Time) list
}
let (|CARGOSTATE|) (cs:CargoState) = cs.factoryCargo, cs.portDeliveries, cs.aDeliveries, cs.bDeliveries

type GlobalState = {
    truck1ReturnToFactoryTime: Time
    truck2ReturnToFactoryTime: Time
    shipReturnToPortTime: Time
    cargoState: CargoState
}
let (|GLOBALSTATE|) (gs:GlobalState) = gs.truck1ReturnToFactoryTime, gs.truck2ReturnToFactoryTime, gs.shipReturnToPortTime, (|CARGOSTATE|)(gs.cargoState)

let newGlobalState truck1ReturnToFactoryTime truck2ReturnToFactoryTime shipReturnToPortTime factoryCargo portDeliveries aDeliveries bDeliveries = {
            truck1ReturnToFactoryTime= truck1ReturnToFactoryTime;
            truck2ReturnToFactoryTime= truck2ReturnToFactoryTime;
            shipReturnToPortTime= shipReturnToPortTime;
            cargoState= { factoryCargo= factoryCargo; portDeliveries= portDeliveries; aDeliveries=aDeliveries; bDeliveries=bDeliveries }
}

let rec transport globalState: GlobalState = 
    match globalState with
        | GLOBALSTATE(tt1, tt2, ts1, (A::fs, pas, aas, bbs)) -> 
            if tt1 <= tt2 then
                transport (newGlobalState (tt1+2) tt2 ts1 fs ((A, tt1+1)::pas) aas bbs)
            else
                transport (newGlobalState tt1 (tt2+2) ts1 fs ((A, tt2+1)::pas) aas bbs)
        | GLOBALSTATE(tt1, tt2, ts1, (B::fs, pas, aas, bbs)) -> 
            if tt1 <= tt2 then
                transport (newGlobalState (tt1+10) tt2 ts1 fs pas aas ((B, tt1+5)::bbs))
            else
                transport (newGlobalState tt1 (tt2+10) ts1 fs pas aas ((A, tt2+5)::bbs))
        | GLOBALSTATE(tt1, tt2, ts1, (fs, (c,ta)::pas, aas, bbs)) -> 
                transport (newGlobalState tt1 tt2 (ts1+8) fs pas ((c, 4+max ts1 ta)::aas) bbs)
        | GLOBALSTATE(tt1, tt2, ts1, ([], [], aas, bbs)) ->
                newGlobalState tt1 tt2 ts1 [] [] aas bbs

let transportAll ls =
    match ls with
        | [] -> 0
        | _ ->  
            let finishedState = transport { truck1ReturnToFactoryTime = 0; truck2ReturnToFactoryTime = 0; shipReturnToPortTime = 0; cargoState = { factoryCargo = ls; portDeliveries = []; aDeliveries = []; bDeliveries = [] } }
            (finishedState.cargoState.aDeliveries @ finishedState.cargoState.bDeliveries) |> List.map snd |> List.reduce max
