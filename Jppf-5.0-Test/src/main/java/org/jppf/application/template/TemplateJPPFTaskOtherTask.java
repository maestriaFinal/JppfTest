/*
 * Copyright 2015 maveces.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.jppf.application.template;

import org.jppf.node.protocol.AbstractTask;

/**
 *
 * @author maveces
 */
public class TemplateJPPFTaskOtherTask extends AbstractTask<String>{
    
    public TemplateJPPFTaskOtherTask(){
        
    }
    
    
    @Override
    public void run(){
          // write your task code here.
        System.out.println("Hola esta es la imprecion de la tarea 2");

    // ...
        // eventually set the execution results
        int s = 100;

        int ss = 1933;

        double sd = s * ss;
        // simply wait for 3 seconds
        String mensaje = "";
        //try {
            //Thread.sleep(3000L);
            mensaje = sd > 100 ? "hola esta es una tarea 2 cierta" : "esta es una tarea 2 falsa";
            System.out.println(mensaje);
//        } catch (InterruptedException e) {
//            setThrowable(e);
//            return;
//        }
// eventually set the execution results

        setResult(mensaje);
    }
}
