import dists


# goal sempre sera 'bucharest'
def a_star(start, goal='Bucharest'):
    """
        Retorna uma lista com o caminho de start até 
        goal segundo o algoritmo A*
    """
    
    
    # Função que retorna uma tupla com o g(n) e f(n)
    def hDLR(cityStart, cityFinal, gOld):
        g = 0
        if (cityStart != cityFinal):
            gSelectList = [x for x in dists.dists[cityStart] if x[0] == cityFinal]
            g = gSelectList[0][1] + gOld
        f = g + dists.straight_line_dists_from_bucharest[cityFinal]
        return (g, f)
    
    # Apenas verificando se a cidade escolhida, é uma opção válida
    if not start in dists.straight_line_dists_from_bucharest:
        print("A cidade escolhida não é uma opção válida:", start)
        print("Tente novamente com uma dos opção abaixo")
        cities = ""
        for city in dists.straight_line_dists_from_bucharest:
            cities = cities + city + " "
        print(cities)
        return
        
    # Definindo a borda inicial
    # O template de um item da borda é: (Nome, g(n), f(n))
    hDLRStart = hDLR(cityStart = start, cityFinal = start, gOld = 0)
    border = ([(start, hDLRStart[0], hDLRStart[1])])
    
    # Inicializando os locais visitados como vazio
    visited = []
    
    # Contador de loops
    count = 1
    
    # Loop infinito, só sai quando encontrar a cidade goal
    while True:
        print(f'Loop {count}')
        count += 1
        
        # No início do loop verificamos se a posição da vez na borda é o goal
        if border[0][0] == goal:
            visited.append(border[0])
            print("Parabéns! Você chegou em " + goal)
            visitedString = ""
            for v in visited:
                visitedString = visitedString + '-> ' + v[0]
            print("Seu caminho foi: " + visitedString)
            print(f'A distancia percorrida foi: {border[0][1]}')
            return
            
            
        
        if len(border) > 0:
            print("Borda antes varrer os filhos", border)
            dadCity = border[0]
            print("Pai da vez: ", dadCity)
            
            borderSons = dists.dists[border[0][0]]
            print("Filhos do pai: ", borderSons)
            
            print("Cidade saindo da borda e indo para os visitados: " + border[0][0])
            visited.append(border[0])
            border.pop(0)
            
            for son in borderSons:
                
                hSon = hDLR(dadCity[0], son[0], dadCity[1])
                sonBorder = (son[0], hSon[0], hSon[1])
                
                isVisited = False
                for v in visited:
                    if son[0] == v[0]:
                        isVisited = True
                
                if not isVisited:
                    if len(border) == 0:
                        border.append(sonBorder)
                    else:
                        for borderItem in border:
                            if not sonBorder in border:
                                border.insert(border.index(borderItem), sonBorder)
                        
                        # Ordenando a borda após inserir os filhos válidos
                        border.sort(key=lambda a: a[2])
        else:
            print("A borda está vazia, ocorreu um erro no processo.")
            return
        
        print("Borda pós varrer os filhos", border)
        print("Visitados", visited)
        print("\n")
