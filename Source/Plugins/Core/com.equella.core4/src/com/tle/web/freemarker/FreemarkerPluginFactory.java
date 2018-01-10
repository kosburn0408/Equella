package com.tle.web.freemarker;

import com.tle.core.plugins.PluginService;
import org.java.plugin.PluginClassLoader;

import javax.inject.Inject;

public class FreemarkerPluginFactory extends PluginFreemarkerFactory {

    @Inject
    private PluginService pluginService;

    @Inject
    public void setSectionsConfiguration(SectionsConfiguration sectionsConfiguration)
    {
        setConfiguration(sectionsConfiguration);
    }

    @Override
    @Inject
    public void setBeanClassLoader(PluginClassLoader classLoader)
    {
        ClassLoader freemarkerLoader = pluginService.getClassLoader("com.tle.web.freemarker");
        super.setBeanClassLoader((PluginClassLoader) freemarkerLoader);
    }
}
