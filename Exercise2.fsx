open System
open System.IO

type Location = Factory | Port | A | B
type Time = int

type Cargo = {
    cargoId: int
    destination: Location
    origin: Location
}

type Event = {
    event: string
    time: Time
    transportId: int
    kind: string
    location: string
    destination: string
    cargo: Cargo list
}

let createEventWithCargo event time transportId kind location destination cargo = {
    event = event;
    time = time;
    transportId = transportId;
    kind = kind;
    location = location;
    destination = destination;
    cargo = [cargo]
}

let createEventWithoutCargo event time transportId kind location destination = {
    event = event;
    time = time;
    transportId = transportId;
    kind = kind;
    location = location;
    destination = destination;
    cargo = []
}

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
    events: Event list
}
let (|GLOBALSTATE|) (gs:GlobalState) = gs.truck1ReturnToFactoryTime, gs.truck2ReturnToFactoryTime, gs.shipReturnToPortTime, (|CARGOSTATE|)(gs.cargoState), gs.events

let newGlobalState truck1ReturnToFactoryTime truck2ReturnToFactoryTime shipReturnToPortTime factoryCargo portDeliveries aDeliveries bDeliveries events = {
            truck1ReturnToFactoryTime = truck1ReturnToFactoryTime;
            truck2ReturnToFactoryTime = truck2ReturnToFactoryTime;
            shipReturnToPortTime = shipReturnToPortTime;
            cargoState = { factoryCargo = factoryCargo; portDeliveries = portDeliveries; aDeliveries = aDeliveries; bDeliveries = bDeliveries }
            events = events;
}

let rec transport globalState: GlobalState = 
    match globalState with
        | GLOBALSTATE(tt1, tt2, ts1, (fs, (a,ta)::pas, aas, bbs), events) -> 
            let transportId = System.Random().Next()
            let departEvent = createEventWithCargo "DEPART" (max ts1 ta) transportId "SHIP" "PORT" "A" a
            let arriveEvent = createEventWithCargo "ARRIVE" (4+max ts1 ta) transportId "SHIP" "A" "" a
            let returnDepartEvent = createEventWithoutCargo "DEPART" (4+max ts1 ta) transportId "SHIP" "A" "PORT"
            let returnArriveEvent = createEventWithoutCargo "ARRIVE" (8+max ts1 ta) transportId "SHIP" "PORT" ""
            transport (newGlobalState tt1 tt2 (8+max ts1 ta) fs pas ((a, 4+max ts1 ta)::aas) bbs (returnArriveEvent::returnDepartEvent::arriveEvent::departEvent::events))
        | GLOBALSTATE(tt1, tt2, ts1, (({ destination = A } as a)::fs, pas, aas, bbs), events) -> 
            if tt1 <= tt2 then
                let transportId = System.Random().Next()
                let departEvent = createEventWithCargo "DEPART" tt1 transportId "TRUCK" "FACTORY" "PORT" a
                let arriveEvent = createEventWithCargo "ARRIVE" (tt1+1) transportId "TRUCK" "PORT" "" a
                let returnDepartEvent = createEventWithoutCargo "DEPART" (tt1+1) transportId "TRUCK" "PORT" "FACTORY"
                let returnArriveEvent = createEventWithoutCargo "ARRIVE" (tt1+2) transportId "TRUCK" "FACTORY" ""
                transport (newGlobalState (tt1+2) tt2 ts1 fs ((a, tt1+1)::pas) aas bbs (returnArriveEvent::returnDepartEvent::arriveEvent::departEvent::events))
            else
                let transportId = System.Random().Next()
                let departEvent = createEventWithCargo "DEPART" tt2 transportId "TRUCK" "FACTORY" "PORT" a
                let arriveEvent = createEventWithCargo "ARRIVE" (tt2+1) transportId "TRUCK" "PORT" "" a
                let returnDepartEvent = createEventWithoutCargo "DEPART" (tt2+1) transportId "TRUCK" "PORT" "FACTORY"
                let returnArriveEvent = createEventWithoutCargo "ARRIVE" (tt2+2) transportId "TRUCK" "FACTORY" ""
                transport (newGlobalState tt1 (tt2+2) ts1 fs ((a, tt2+1)::pas) aas bbs (returnArriveEvent::returnDepartEvent::arriveEvent::departEvent::events))
        | GLOBALSTATE(tt1, tt2, ts1, (({ destination = B } as b)::fs, pas, aas, bbs), events) -> 
            if tt1 <= tt2 then
                let transportId = System.Random().Next()
                let departEvent = createEventWithCargo "DEPART" tt1 transportId "TRUCK" "FACTORY" "B" b
                let arriveEvent = createEventWithCargo "ARRIVE" (tt1+5) transportId "TRUCK" "B" "" b
                let returnDepartEvent = createEventWithoutCargo "DEPART" (tt1+5) transportId "TRUCK" "B" "FACTORY"
                let returnArriveEvent = createEventWithoutCargo "ARRIVE" (tt1+10) transportId "TRUCK" "FACTORY" ""
                transport (newGlobalState (tt1+10) tt2 ts1 fs pas aas ((b, tt1+5)::bbs) (returnArriveEvent::returnDepartEvent::arriveEvent::departEvent::events))
            else
                let transportId = System.Random().Next()
                let departEvent = createEventWithCargo "DEPART" tt2 transportId "TRUCK" "FACTORY" "B" b
                let arriveEvent = createEventWithCargo "ARRIVE" (tt2+5) transportId "TRUCK" "B" "" b
                let returnDepartEvent = createEventWithoutCargo "DEPART" (tt2+5) transportId "TRUCK" "B" "FACTORY"
                let returnArriveEvent = createEventWithoutCargo "ARRIVE" (tt2+10) transportId "TRUCK" "FACTORY" ""
                transport (newGlobalState tt1 (tt2+10) ts1 fs pas aas ((b, tt2+5)::bbs) (returnArriveEvent::returnDepartEvent::arriveEvent::departEvent::events))
        | GLOBALSTATE(tt1, tt2, ts1, ([], [], aas, bbs), events) ->
                newGlobalState tt1 tt2 ts1 [] [] aas bbs events

let printEvent file event =
    if event.cargo.IsEmpty
        then fprintfn file "{\"event\": \"%s\", \"time\": %i, \"transport_id\": %i, \"kind\": \"%s\", \"location\": \"%s\", \"destination\": \"%s\", \"cargo\": []}" event.event event.time event.transportId event.kind event.location event.destination
    else 
        fprintfn file "{\"event\": \"%s\", \"time\": %i, \"transport_id\": %i, \"kind\": \"%s\", \"location\": \"%s\", \"destination\": \"%s\", \"cargo\": [{\"cargo_id\": %i, \"destination\": \"%s\", \"origin\": \"%s\"}]}" event.event event.time event.transportId event.kind event.location event.destination (event.cargo.Item(0).cargoId) (event.cargo.Item(0).destination.ToString()) (event.cargo.Item(0).origin.ToString())

let logEvents filename events = 
    let filePath = __SOURCE_DIRECTORY__ + "\\logs\\" + filename
    use file = System.IO.File.CreateText(filePath)
    List.iter (printEvent file) events
    file.Close()

let buildCargo id l =
    match l with  
        | A -> { cargoId = id; destination = A; origin = Factory } 
        | B -> { cargoId = id; destination = B; origin = Factory }
        | Port -> { cargoId = id; destination = Port; origin = Factory }
        | Factory -> { cargoId = id; destination = Factory; origin = Factory }

let buildAllCargo ls =
    List.mapi buildCargo ls

let transportAll sls ls =
    match ls with
        | [] -> 0
        | _ ->  
            let cargo = buildAllCargo ls
            let initialState = newGlobalState 0 0 0 cargo [] [] [] []
            let finishedState = transport initialState
            let events = List.sortBy (fun event -> 10 * event.time + (if event.event = "DEPART" then 1 else 0)) finishedState.events
            logEvents (sls + ".txt") events
            (finishedState.cargoState.aDeliveries @ finishedState.cargoState.bDeliveries) |> List.map snd |> List.reduce max

let transportAllStringInput (sls: string) =
    let ls = sls.ToCharArray() |> Seq.map (function
            | 'A' -> A
            | 'B' -> B
            | _ -> failwithf "Unknown cargo") |> Seq.toList
    transportAll sls ls
   