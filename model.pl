%application(appId, listOfHWs, listOfSWs)
application(iotApp1, [network1, network2, thermostat, lights, camera, lock, disk],[userConfig, businessLogic, authentication, aiLearning, network1Controller, 
                      network2Controller, thermostatController, lightsController, cameraController, lockController, diskController]).
application(iotApp2, [network1, network2, thermostat2, lights, camera, lock, disk],[userConfig, businessLogic, authentication, aiLearning, network1Controller, 
                      network2Controller, thermostatController, lightsController, cameraController, lockController, diskController]).
application(iotApp3, [network1, network2, thermostat, lights, camera, lock, disk],[userConfig, businessLogic, authentication, aiLearning, network1Controller, 
                      network2Controller, thermostatController, lightsController, cameraController, lockController, diskController2 ]).
application(iotApp4, [network1, network2, thermostat2, lights, camera, lock, disk],[userConfig, businessLogic, authentication, aiLearning, network1Controller, 
                      network2Controller, thermostatController, lightsController, cameraController, lockController, diskController2 ]).
application(iotApp5, [network1, network2, thermostat, lights, camera, lock, disk],[userConfig, businessLogic, authentication, aiLearning2, network1Controller, 
                      network2Controller, thermostatController, lightsController2, cameraController, lockController,diskController]).

%software(componentId, ListOfData,ListOfReqs,Cost,LinkedComponents)
%hardware(componentId, ListOfData,ListOfCharacteristics,LinkedComponents)
software( userConfig, 
            [wantedTemp, wantedBright, cameraActivation, schedule, aiActive], %data
            [],% ListOfCharacteristics
            30, %work hour to change partition
            ([],[aiLearning, businessLogic, authentication, diskController]) %linked components (hw,sw)
            ).

software( businessLogic,
            [credentials],
            %[wantedTemp, wantedBright, cameraActivation, schedule, aiActive, thermostatTemp, lightBright,
            %    cameraStream, thermostatStatus, lightStatus, cameraStatus, lockStatus, lockCommands,
            %    actuationCommands, cameraLive], 
            [],
            75,
            ([],[aiLearning, userConfig, authentication]) 
            ).

software( authentication,
            [credentials,wantedTemp], 
            %[wantedTemp, wantedBright, cameraActivation, schedule, aiActive, thermostatTemp, lightBright,
            %    cameraStream, thermostatStatus, lightStatus, cameraStatus, lockStatus, lockCommands,
            %    actuationCommands, cameraLive, credentials], 
            [tlsLibrary], 
            45,
            ([],[userConfig, businessLogic, network1Controller, diskController]) 
            ).

software( thermostatController, 
            %[wantedTemp, thermostatTemp, actuationCommands], 
            [wantedTemp],
            [], 
            40, 
            ([],[businessLogic, network2Controller,lightsController])
            ).

software( lightsController, 
            [wantedBright, lightBright, actuationCommands], 
            [ligthDriver], 
            40, 
            ([],[businessLogic, network2Controller,thermostatController]) 
            ).
    
software( lightsController2, 
            [wantedBright, lightBright, actuationCommands], 
            [ligthDriver], 
            40, 
            ([],[businessLogic, network2Controller, network1Controller]) 
            ).

software( cameraController, 
            [cameraActivation, cameraLive, cameraStatus, cameraStream], 
            [], 
            45, 
            ([],[businessLogic, network2Controller]) 
            ).

software( lockController, 
            [lockCommands, lockStatus], 
            [], 
            50, 
            ([],[businessLogic, network2Controller]) 
            ).

software( network1Controller, 
            [networkInput, networkOutput], 
            [networkDriver],
            25, 
            ([network1],[authentication]) 
            ).

software( network2Controller, 
            [wantedTemp, wantedBright, cameraActivation, thermostatTemp, lightBright,
                cameraStream, thermostatStatus, lightStatus, cameraStatus, lockStatus, lockCommands,
                actuationCommands, cameraLive], 
            [],
            25, 
            ([network2],[thermostatController, lightsController, lockController, cameraController, diskController]) 
            ).

software( diskController, 
            [schedule, credentials, wantedTemp, wantedBright, cameraActivation, aiActive], 
            [], 
            25, 
            ([disk],[userConfig, authentication, network2Controller]) 
            ).

software( aiLearning, 
            [wantedTemp, wantedBright, cameraActivation, schedule, aiActive,thermostatTemp, lightBright, cameraStream], 
            [aiFramework], 
            55, 
            ([],[userConfig, businessLogic]) 
            ).
software( aiLearning2, 
            [wantedTemp, wantedBright, cameraActivation, schedule, aiActive,thermostatTemp, lightBright, cameraStream], 
            [aiFramework], 
            55, 
            ([],[userConfig, businessLogic,lightsController2]) 
            ).

software( diskController2, 
            [schedule, credentials, wantedTemp, wantedBright, cameraActivation, aiActive], 
            [diskDriver], 
            25, 
            ([disk],[businessLogic]) 
            ).

software( diskDeclassifier, 
            [schedule, credentials, wantedTemp, wantedBright, cameraActivation, aiActive], 
            [], 
            55, 
            ([disk],[diskController]) 
            ).

%%%%%Hardware componenents

hardware( network1, 
            [networkInput, networkOutput], 
            [openSystem], %characteristics
            [authentication] 
            ).
hardware( network2, 
            [networkInput, networkOutput,schedule], 
            [], %characteristics
            [network2Controller, thermostat, lights, camera, lock] 
            ).
hardware( thermostat, 
            [thermostatStatus, thermostatTemp, actuationCommands], 
            [], %characteristics
            [network2] 
            ).
hardware( lights, 
            [lightStatus, lightBright, actuationCommands], 
            [], %characteristics
            [network2] 
            ).
hardware( camera, 
            [cameraActivation, cameraStatus, cameraStream], 
            [], %characteristics
            [network2] 
            ).
hardware( lock, 
            [lockStatus, lockCommands], 
            [], %characteristics
            [network2] 
            ).
hardware( disk, 
            [storage], 
            [sharedDisk], %characteristics
            [diskController] 
            ).

hardware( thermostat2, 
            [thermostatStatus, thermostatTemp, actuationCommands], 
            [openSystem], %characteristics
            [network2] 
            ).

% lattice of security 
%g_lattice_higherThan(higherElement, lowerElement)
g_lattice_higherThan(top, medium).
g_lattice_higherThan(medium, low).

%policies for data
%tag(variable, security level)
tag(wantedTemp, medium).
tag(wantedBright, medium).
tag(cameraActivation, top).
tag(schedule, top).
tag(aiActive, low).
tag(networkInput, low).
tag(networkOutput, low).
tag(thermostatTemp, low).
tag(lightBright, low).
tag(cameraStream, top).
tag(thermostatStatus, medium).
tag(lightStatus, medium).
tag(cameraStatus, top).
tag(lockStatus, top).
tag(lockCommands, top).
tag(actuationCommands, medium).
tag(cameraLive, top).
tag(credentials, top).
tag(storage, low).

%policies for characteristics
tag(tlsLibrary, top).
tag(openSystem, low).
tag(networkDriver, low).
tag(aiFramework, low).
tag(sharedDisk, low).
tag(diskDriver, low).
tag(ligthDriver, medium).