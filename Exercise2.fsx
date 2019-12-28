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
    cargo = cargo
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
        | GLOBALSTATE(tt1, tt2, ts1, (({ destination = A } as a)::fs, pas, aas, bbs), events) -> 
            if tt1 <= tt2 then
                let transportId = System.Random().Next()
                let loadEvent = createEventWithCargo "LOAD" tt1 transportId "TRUCK" "FACTORY" "PORT" [a]
                let departEvent = createEventWithCargo "DEPART" tt1 transportId "TRUCK" "FACTORY" "PORT" [a]
                let arriveEvent = createEventWithCargo "ARRIVE" (tt1+1) transportId "TRUCK" "PORT" "" [a]
                let unloadEvent = createEventWithCargo "UNLOAD" (tt1+1) transportId "TRUCK" "PORT" "" [a]
                let returnDepartEvent = createEventWithCargo "DEPART" (tt1+1) transportId "TRUCK" "PORT" "FACTORY" []
                let returnArriveEvent = createEventWithCargo "ARRIVE" (tt1+2) transportId "TRUCK" "FACTORY" "" []
                transport (newGlobalState (tt1+2) tt2 ts1 fs ((a, tt1+1)::pas) aas bbs (returnArriveEvent::returnDepartEvent::unloadEvent::arriveEvent::departEvent::loadEvent::events))
            else
                let transportId = System.Random().Next()
                let loadEvent = createEventWithCargo "LOAD" tt2 transportId "TRUCK" "FACTORY" "PORT" [a]
                let departEvent = createEventWithCargo "DEPART" tt2 transportId "TRUCK" "FACTORY" "PORT" [a]
                let arriveEvent = createEventWithCargo "ARRIVE" (tt2+1) transportId "TRUCK" "PORT" "" [a]
                let unloadEvent = createEventWithCargo "UNLOAD" (tt2+1) transportId "TRUCK" "PORT" "" [a]
                let returnDepartEvent = createEventWithCargo "DEPART" (tt2+1) transportId "TRUCK" "PORT" "FACTORY" []
                let returnArriveEvent = createEventWithCargo "ARRIVE" (tt2+2) transportId "TRUCK" "FACTORY" "" []
                transport (newGlobalState tt1 (tt2+2) ts1 fs ((a, tt2+1)::pas) aas bbs (returnArriveEvent::returnDepartEvent::unloadEvent::arriveEvent::departEvent::loadEvent::events))
        | GLOBALSTATE(tt1, tt2, ts1, (({ destination = B } as b)::fs, pas, aas, bbs), events) -> 
            if tt1 <= tt2 then
                let transportId = System.Random().Next()
                let loadEvent = createEventWithCargo "LOAD" tt1 transportId "TRUCK" "FACTORY" "B" [b]
                let departEvent = createEventWithCargo "DEPART" tt1 transportId "TRUCK" "FACTORY" "B" [b]
                let arriveEvent = createEventWithCargo "ARRIVE" (tt1+5) transportId "TRUCK" "B" "" [b]
                let unloadEvent = createEventWithCargo "UNLOAD" (tt1+5) transportId "TRUCK" "B" "" [b]
                let returnDepartEvent = createEventWithCargo "DEPART" (tt1+5) transportId "TRUCK" "B" "FACTORY" []
                let returnArriveEvent = createEventWithCargo "ARRIVE" (tt1+10) transportId "TRUCK" "FACTORY" "" []
                transport (newGlobalState (tt1+10) tt2 ts1 fs pas aas ((b, tt1+5)::bbs) (returnArriveEvent::returnDepartEvent::unloadEvent::arriveEvent::departEvent::loadEvent::events))
            else
                let transportId = System.Random().Next()
                let loadEvent = createEventWithCargo "LOAD" tt2 transportId "TRUCK" "FACTORY" "B" [b]
                let departEvent = createEventWithCargo "DEPART" tt2 transportId "TRUCK" "FACTORY" "B" [b]
                let arriveEvent = createEventWithCargo "ARRIVE" (tt2+5) transportId "TRUCK" "B" "" [b]
                let unloadEvent = createEventWithCargo "UNLOAD" (tt2+5) transportId "TRUCK" "B" "" [b]
                let returnDepartEvent = createEventWithCargo "DEPART" (tt2+5) transportId "TRUCK" "B" "FACTORY" []
                let returnArriveEvent = createEventWithCargo "ARRIVE" (tt2+10) transportId "TRUCK" "FACTORY" "" []
                transport (newGlobalState tt1 (tt2+10) ts1 fs pas aas ((b, tt2+5)::bbs) (returnArriveEvent::returnDepartEvent::unloadEvent::arriveEvent::departEvent::loadEvent::events))
        | GLOBALSTATE(tt1, tt2, ts1, (fs, pas, aas, bbs), events) when not (List.isEmpty pas) -> 
            let transportId = System.Random().Next()
            let earliestDepartureTime = max ts1 (List.minBy (snd) pas |> snd)
            let sortedCargo = List.sortBy (snd) pas
            let shippableCargo = List.ofSeq (Seq.takeWhile (fun (_,t) -> t <= earliestDepartureTime) sortedCargo)
            let cargoToShip = List.ofSeq (Seq.take (min 4 (Seq.length shippableCargo)) shippableCargo)
            let remainingCargo = List.ofSeq (Seq.skip (Seq.length cargoToShip) sortedCargo)
            let unloadedTime = 1+6+1+earliestDepartureTime
            let cargoDeliveries = List.map (fun (c,_) -> (c, unloadedTime)) cargoToShip

            let loadEvent = createEventWithCargo "LOAD" earliestDepartureTime transportId "SHIP" "PORT" "A" (List.map fst cargoDeliveries)
            let departEvent = createEventWithCargo "DEPART" (1 + earliestDepartureTime) transportId "SHIP" "PORT" "A" (List.map fst cargoDeliveries)
            let arriveEvent = createEventWithCargo "ARRIVE" (1+6+earliestDepartureTime) transportId "SHIP" "A" "" (List.map fst cargoDeliveries)
            let unloadEvent = createEventWithCargo "UNLOAD" (1+6+earliestDepartureTime) transportId "SHIP" "A" "" (List.map fst cargoDeliveries)
            let returnDepartEvent = createEventWithCargo "DEPART" (unloadedTime) transportId "SHIP" "A" "PORT" []
            let returnArriveEvent = createEventWithCargo "ARRIVE" (1+6+1+6+earliestDepartureTime) transportId "SHIP" "PORT" "" []
            transport (newGlobalState tt1 tt2 (1+6+1+6+earliestDepartureTime) fs remainingCargo (cargoDeliveries @ aas) bbs (returnArriveEvent::returnDepartEvent::unloadEvent::arriveEvent::departEvent::loadEvent::events))
        | GLOBALSTATE(tt1, tt2, ts1, ([], [], aas, bbs), events) ->
            newGlobalState tt1 tt2 ts1 [] [] aas bbs events

let printEvent file event =
    let cargoList = 
        if event.cargo.IsEmpty 
            then ""
        else
            let cargoStrings = List.map (fun c -> sprintf "{\"cargo_id\": %i, \"destination\": \"%s\", \"origin\": \"%s\"}" c.cargoId (c.destination.ToString()) (c.origin.ToString())) event.cargo
            String.concat "," cargoStrings

    fprintfn file "{\"event\": \"%s\", \"time\": %i, \"transport_id\": %i, \"kind\": \"%s\", \"location\": \"%s\", \"destination\": \"%s\", \"cargo\": [%s]}" event.event event.time event.transportId event.kind event.location event.destination cargoList

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

let getOrder eventType =
    match eventType with
        | "ARRIVE" -> 1
        | "UNLOAD" -> 2
        | "LOAD" -> 3
        | "DEPART" -> 4
        | _ -> 9

let transportAll sls ls =
    match ls with
        | [] -> 0
        | _ ->  
            let cargo = buildAllCargo ls
            let initialState = newGlobalState 0 0 0 cargo [] [] [] []
            let finishedState = transport initialState
            let events = List.sortBy (fun event -> 10 * event.time + getOrder event.event) finishedState.events
            logEvents (sls + ".txt") events
            (finishedState.cargoState.aDeliveries @ finishedState.cargoState.bDeliveries) |> List.map snd |> List.reduce max

let transportAllStringInput (sls: string) =
    let ls = sls.ToCharArray() |> Seq.map (function
            | 'A' -> A
            | 'B' -> B
            | _ -> failwithf "Unknown cargo") |> Seq.toList
    transportAll sls ls
   