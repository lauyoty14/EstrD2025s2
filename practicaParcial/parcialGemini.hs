ganarEsmelarlda :: EmGame -> Personaje -> Personaje -> Esmeralda -> EmGame
ganarEsmeralda (AG mapP mapE h) p1 p2 e =
    case lookupM p1 mapP of
        Nothing -> error "personaje 1 no existe"
        Just p1es -> case lookupM p2 mapP of
            Nothing -> error "personaje 2 no existe"
            Just p2es -> case lookupM e mapE of
                Nothing -> error "esmeralda no existe"
                Just _ -> if elem e (p1es ++ p2es) && (poder p1) >= (poder p2)
                            then AG (assocM p1 (e:p1es) (assocM p2 (listaSin e p2es) mapP))
                                    (assocM e p1 mapE) h
                            else AG (assocM p2 (e:p2es) (assocM p1 (listaSin e p1es) mapP))
                                    (assocM e p2 mapE) h