package org.jppf.application.template;

import org.jppf.node.protocol.AbstractTask;

/**
 *
 * @author maveces
 */
public class TemplateJPPFTaskOtherTask extends AbstractTask<String> {

    public TemplateJPPFTaskOtherTask() {

    }

    @Override
    public void run() {
        System.out.println("Hola esta es la imprecion de la tarea 2");
        int s = 100;
        int ss = 1933;
        double sd = s * ss;
        String mensaje = "";
        mensaje = sd > 100 ? "hola esta es una tarea 2 cierta" : "esta es una tarea 2 falsa";
        System.out.println(mensaje);
        setResult(mensaje);
    }
}
