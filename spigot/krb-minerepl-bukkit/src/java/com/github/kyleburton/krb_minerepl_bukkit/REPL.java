package com.github.kyleburton.krb_minerepl_bukkit;

import org.bukkit.plugin.java.JavaPlugin;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import clojure.java.api.Clojure;
import clojure.lang.IFn;


public final class REPL extends JavaPlugin {
    public static final Logger LOG                       = LoggerFactory.getLogger(REPL.class.getName());
    public static final String DEFAULT_REPL_PORT         = "4123";
    public static final String DEFAULT_REPL_BIND_ADDRESS = "127.0.0.1";

    public static Object server = null;
    public static REPL plugin = null;

    public static void cljRequire(String ns) {
        IFn require = Clojure.var("clojure.core", "require");
        LOG.info("    ... clojure.core/require %s".format(ns));
        require.invoke(Clojure.read(ns));
    }

    public static Object cljEval(String code) {
        IFn eval = Clojure.var("clojure.core", "eval");
        return eval.invoke(Clojure.read(code));
    }

    public static String getBindAddress() {
        return System.getProperty("com.github.kyleburton.krb_minerepl_bukkit.listenAddress", DEFAULT_REPL_BIND_ADDRESS);
    }

    public static String getBindPort() {
        return System.getProperty("com.github.kyleburton.krb_minerepl_bukkit.listenPort", DEFAULT_REPL_PORT);
    }

    @Override
    public void onEnable() {
        LOG.info("com.github.kyleburton.krb_minerepl_bukkit.REPL: onEnable!");
        plugin = this;

        synchronized (this) {
            if (server == null) {
                LOG.info("    ... starting cider nrepl: %s:%s".format(DEFAULT_REPL_BIND_ADDRESS, DEFAULT_REPL_PORT));
                LOG.info("classpath is: '%s'".format(System.getProperty("java.class.path")));
                try {
                    Class clazz;
                    clazz = Class.forName("clojure.core$max");
                    LOG.info("got clojure.core/max", clazz);
                    clazz = Class.forName("clojure.core__init");
                    LOG.info("got clojure.core__init=%s".format(clazz.toString()));
                }
                catch (ClassNotFoundException ex) {
                    LOG.error("Error: not found :(", ex);
                    throw new RuntimeException(ex);
                }
                catch (Throwable ex) {
                    LOG.error("Error: throwable?", ex);
                    throw ex;
                }
                cljRequire("nrepl.server");
                cljRequire("cider.nrepl");
                String expr = "(nrepl.server/start-server"
                    + "\n  :port " + DEFAULT_REPL_PORT
                    + "\n  :bind \"" + DEFAULT_REPL_BIND_ADDRESS + "\""
                    + "\n  :handler cider.nrepl/cider-nrepl-handler)";
                LOG.info("expression=%s".format(expr));
                LOG.info("starting server on bind-address=%s:%s".format(DEFAULT_REPL_BIND_ADDRESS, DEFAULT_REPL_PORT));
                server = cljEval(expr);

                // cljRequire("krb_minerepl_bukkit.core");
                // IFn setPlugin = Clojure.var("krb-minerepl-bukkit.core", "set-plugin!");
                // setPlugin.invoke(this);

            }
        }


    }

    @Override
    public void onDisable() {
        LOG.info("com.github.kyleburton.krb_minerepl_bukkit.REPL: onDisable!");

        synchronized (this) {
            if (server != null) {
                // stop the server somehow?
                LOG.error("There is a server, how do we stop the server then?");
            }
        }

    }
}
