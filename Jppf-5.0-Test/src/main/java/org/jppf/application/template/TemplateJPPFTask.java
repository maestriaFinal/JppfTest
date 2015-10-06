package org.jppf.application.template;

import org.jppf.node.protocol.AbstractTask;
public class TemplateJPPFTask extends AbstractTask<String> {
    public TemplateJPPFTask() {
        // perform initializations here ...
    }
    @Override
    public void run() {
        System.out.println("Hola esta es la imprecion de la tarea 1");
        int s = 100;

        int ss = 1933;

        double sd = s * ss;
        String mensaje = "";
        try {
            Thread.sleep(3000L);
            mensaje = sd > 100 ? "hola esta es una tarea cierta" : "esta es una tarea falsa";
            System.out.println(mensaje);
        } catch (InterruptedException e) {
            setThrowable(e);
            return;
        }
        setResult(mensaje);
    }
}
