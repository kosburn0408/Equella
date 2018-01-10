/*
 * Copyright 2017 Apereo
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.tle.freetext;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import javax.inject.Singleton;

import org.apache.log4j.Logger;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.NRTManager;
import org.springframework.transaction.annotation.Transactional;

import com.dytech.devlib.PropBagEx;
import com.dytech.edge.exceptions.InvalidSearchQueryException;
import com.dytech.edge.exceptions.SearchingException;
import com.google.common.collect.Multimap;
import com.google.inject.Inject;
import com.google.inject.name.Named;
import com.tle.beans.Institution;
import com.tle.beans.item.Item;
import com.tle.beans.item.ItemPack;
import com.tle.beans.item.ItemSelect;
import com.tle.common.Pair;
import com.tle.common.i18n.CurrentLocale;
import com.tle.common.searching.Search;
import com.tle.common.searching.SearchResults;
import com.tle.common.settings.standard.SearchSettings;
import com.tle.core.guice.Bind;
import com.tle.core.healthcheck.listeners.ServiceCheckRequestListener;
import com.tle.core.healthcheck.listeners.ServiceCheckResponseListener.CheckServiceResponseEvent;
import com.tle.core.healthcheck.listeners.bean.ServiceStatus;
import com.tle.core.healthcheck.listeners.bean.ServiceStatus.ServiceName;
import com.tle.core.healthcheck.listeners.bean.ServiceStatus.Status;
import com.tle.core.institution.RunAsInstitution;
import com.tle.core.institution.events.InstitutionEvent;
import com.tle.core.institution.events.InstitutionEvent.InstitutionEventType;
import com.tle.core.institution.events.listeners.InstitutionListener;
import com.tle.core.item.dao.ItemDao;
import com.tle.core.item.helper.ItemHelper;
import com.tle.core.item.service.ItemService;
import com.tle.core.plugins.PluginService;
import com.tle.core.plugins.PluginTracker;
import com.tle.core.remoting.MatrixResults;
import com.tle.core.events.services.EventService;
import com.tle.core.freetext.index.ItemIndex;
import com.tle.core.freetext.index.AbstractIndexEngine.IndexBuilder;
import com.tle.core.freetext.indexer.IndexingExtension;
import com.tle.core.services.item.FreetextResult;
import com.tle.core.services.user.UserPreferenceService;
import com.tle.core.settings.service.ConfigurationService;
import com.tle.core.zookeeper.ZookeeperService;

import it.uniroma3.mat.extendedset.wrappers.LongSet;

/**
 * @author jmaginnis
 */
@Bind(FreetextIndex.class)
@Singleton
@SuppressWarnings("nls")
public class FreetextIndexImpl implements FreetextIndex, InstitutionListener, ServiceCheckRequestListener
{
	private static final Logger LOGGER = Logger.getLogger(FreetextIndexImpl.class);

	private final File indexPath;

	@Inject
	private ConfigurationService configConstants;
	@Inject
	private ItemDao itemDao;
	@Inject
	private ItemService itemService;
	@Inject
	private ItemHelper itemHelper;
	@Inject
	private RunAsInstitution runAs;
	@Inject
	private EventService eventService;
	@Inject
	private ZookeeperService zkService;

	@Inject(optional = true)
	@Named("freetextIndex.defaultOperator")
	private String defaultOperator = "AND";
	@Inject(optional = true)
	@Named("freetextIndex.synchroiseMinutes")
	private int synchroniseMinutes = 5;

	@Inject
	@Named("freetext.stopwords.file")
	private File stopWordsFile;

	@Inject
	private UserPreferenceService userPrefs;

	private int maxBooleanClauses = 8192;

	private PluginTracker<IndexingExtension> indexingTracker;
	private PluginTracker<ItemIndex<? extends FreetextResult>> indexTracker;

	private boolean indexesHaveBeenInited;

	@Inject
	public FreetextIndexImpl(@Named("freetext.index.location") File indexPath)
	{
		this.indexPath = indexPath;
	}

	public File getIndexPath()
	{
		return this.indexPath;
	}

	/**
	 * Invoked by Spring framework.
	 */
	public void setSynchroniseMinutes(int synchroniseMinutes)
	{
		this.synchroniseMinutes = synchroniseMinutes;
	}

	/**
	 * For backwards compatibility.
	 * 
	 * @param synchroniseMinutes
	 */
	public void setSynchroiseMinutes(int synchroniseMinutes)
	{
		this.synchroniseMinutes = synchroniseMinutes;
	}

	private Collection<ItemIndex<? extends FreetextResult>> getAllIndexes()
	{
		return getIndexerMap().values();
	}

	@Override
	public SearchSettings getSearchSettings()
	{
		return configConstants.getProperties(new SearchSettings());
	}

	private boolean isSearchAttachment()
	{
		return userPrefs.isSearchAttachment();
	}

	/**
	 * @see com.dytech.edge.search.FTE#search(java.lang.String)
	 * @throws InvalidSearchQueryException
	 */
	@SuppressWarnings("unchecked")
	@Override
	public <T extends FreetextResult> SearchResults<T> search(Search searchReq, int start, int count)

	{
		try
		{
			return (SearchResults<T>) getIndexer(searchReq.getSearchType()).search(searchReq, start, count,
				isSearchAttachment());
		}
		catch( SearchingException ex )
		{
			if( !ex.isLogged() )
			{
				LOGGER.error(ex);
			}
			throw ex;
		}
	}

	@Override
	public LongSet searchBitSet(Search searchReq)
	{
		try
		{
			return getIndexer(searchReq.getSearchType()).searchBitSet(searchReq, isSearchAttachment());
		}
		catch( InvalidSearchQueryException iqe )
		{
			throw iqe;
		}
		catch( SearchingException ex )
		{
			if( !ex.isLogged() )
			{
				LOGGER.error(ex);
			}
			throw ex;
		}
	}

	/*
	 * @see com.dytech.edge.search.FTE#count(java.lang.String)
	 * @throws InvalidSearchQueryException
	 */
	@Override
	public int count(Search searchReq)
	{
		try
		{
			boolean searchNotInAttachment = isSearchAttachment();
			return getIndexer(searchReq.getSearchType()).count(searchReq, searchNotInAttachment);
		}
		catch( SearchingException ex )
		{
			if( !ex.isLogged() )
			{
				LOGGER.error(ex);
			}
			throw ex;
		}
	}

	@Override
	public ItemIndex<? extends FreetextResult> getIndexer(String index)
	{
		return getIndexerMap().get(index);
	}

	@Override
	public synchronized void deleteIndexes()
	{
		Map<String, ItemIndex<? extends FreetextResult>> indexers = indexTracker.getBeanMap();
		for( ItemIndex<? extends FreetextResult> index : indexers.values() )
		{
			index.deleteDirectory();
		}
		indexesHaveBeenInited = false;
	}

	private synchronized Map<String, ItemIndex<? extends FreetextResult>> getIndexerMap()
	{
		Map<String, ItemIndex<? extends FreetextResult>> indexerMap = indexTracker.getBeanMap();
		if( !indexesHaveBeenInited )
		{
			// This simply exists in order to reset the indices for the first
			// time.
			IndexBuilder resetBuilder = new IndexBuilder()
			{
				@Override
				public long buildIndex(NRTManager nrtManager) throws Exception
				{
					return nrtManager.getCurrentSearchingGen(true);
				}
			};
			for( ItemIndex<? extends FreetextResult> itemIndex : indexerMap.values() )
			{
				itemIndex.modifyIndex(resetBuilder);
			}
			indexesHaveBeenInited = true;
		}
		return indexerMap;
	}

	@Override
	public Multimap<String, Pair<String, Integer>> facetCount(Search search, Collection<String> fields)
	{
		return getIndexer(Search.INDEX_ITEM).facetCount(search, fields);
	}

	@Override
	public MatrixResults matrixSearch(Search search, List<String> fields, boolean countOnly)
	{
		return getIndexer(Search.INDEX_ITEM).matrixSearch(search, fields, countOnly);
	}

	public int getMaxBooleanClauses()
	{
		return maxBooleanClauses;
	}

	public void setMaxBooleanClauses(int maxBooleanClauses)
	{
		this.maxBooleanClauses = maxBooleanClauses;
		BooleanQuery.setMaxClauseCount(maxBooleanClauses);
	}

	@Override
	public void indexBatch(List<IndexedItem> batch)
	{
		Collection<ItemIndex<?>> allIndexes = getAllIndexes();
		for( ItemIndex<?> itemIndex : allIndexes )
		{
			itemIndex.indexBatch(batch);
		}
	}

	@Override
	public Collection<IndexingExtension> getIndexingExtensions()
	{
		Map<String, IndexingExtension> beans = indexingTracker.getBeanMap();
		return beans.values();
	}

	@Inject
	public void setPluginService(PluginService pluginService)
	{
		indexingTracker = new PluginTracker<IndexingExtension>(pluginService, "com.tle.core.freetext", "indexingExtension", null); //$NON-NLS-1$
		indexingTracker.setBeanKey("class"); //$NON-NLS-1$
		indexTracker = new PluginTracker<ItemIndex<?>>(pluginService, "com.tle.core.freetext", "freetextIndex", "id"); //$NON-NLS-1$ //$NON-NLS-2$
		indexTracker.setBeanKey("class"); //$NON-NLS-1$
	}

	@Transactional(readOnly = true)
	@Override
	public void prepareItemsForIndexing(Collection<IndexedItem> indexitems)
	{
		Collection<IndexingExtension> extensions = getIndexingExtensions();
		ItemSelect select = new ItemSelect();
		for( IndexingExtension indexingExtension : extensions )
		{
			indexingExtension.prepareForLoad(select);
		}
		ArrayList<Long> idList = new ArrayList<Long>();
		for( IndexedItem item : indexitems )
		{
			idList.add(item.getItemIdKey().getKey());
		}
		List<Item> items = itemDao.getItems(idList, select, null);
		List<IndexedItem> existantItems = new ArrayList<IndexedItem>();
		int i = 0;
		for( final IndexedItem inditem : indexitems )
		{
			final Item item = items.get(i++);
			try
			{
				if( item != null )
				{
					inditem.setItem(item);
					runAs.executeAsSystem(item.getInstitution(), new Callable<Void>()
					{
						@Override
						public Void call()
						{
							PropBagEx itemxml = itemService.getItemXmlPropBag(item);
							ItemPack itemPack = new ItemPack();
							itemPack.setItem(item);
							itemPack.setXml(itemxml);
							inditem
								.setItemXml(itemHelper.convertToXml(itemPack, new ItemHelper.ItemHelperSettings(true)));
							return null;
						}
					});
					existantItems.add(inditem);
				}
				else
				{
					inditem.setAdd(false);
				}
			}
			catch( Exception e )
			{
				LOGGER.error("Error getting xml for " + item, e); //$NON-NLS-1$
				inditem.setError(e);
			}
		}
		for( IndexingExtension indexingExtension : extensions )
		{
			indexingExtension.loadForIndexing(existantItems);
		}
		for( final IndexedItem inditem : indexitems )
		{
			inditem.setPrepared(true);
		}
	}

	public void setDefaultOperator(String defaultOperator)
	{
		this.defaultOperator = defaultOperator;
	}

	@Override
	public File getStopWordsFile()
	{
		return stopWordsFile;
	}

	@Override
	public String getDefaultOperator()
	{
		return defaultOperator;
	}

	@Override
	public File getRootIndexPath()
	{
		return indexPath;
	}

	@Override
	public int getSynchroniseMinutes()
	{
		return synchroniseMinutes;
	}

	@Override
	public void institutionEvent(InstitutionEvent event)
	{
		if( event.getEventType() == InstitutionEventType.DELETED )
		{
			Collection<Institution> insts = event.getChanges().values();
			for( Institution institution : insts )
			{
				Collection<ItemIndex<?>> allIndexes = getAllIndexes();
				for( ItemIndex<?> itemIndex : allIndexes )
				{
					itemIndex.deleteForInstitution(institution.getUniqueId());
				}
			}
		}
	}

	/**
	 * @see com.tle.freetext.FreetextIndex#suggestTerm(com.tle.common.searching.Search,
	 *      java.lang.String)
	 * @throws InvalidSearchQueryException
	 */
	@Override
	public String suggestTerm(Search request, String prefix)
	{
		try
		{
			return getIndexer("item").suggestTerm(request, prefix, isSearchAttachment());
		}
		catch( SearchingException ex )
		{
			if( !ex.isLogged() )
			{
				LOGGER.error(ex);
			}
			throw ex;
		}
	}

	@Override
	public void checkServiceRequest(CheckServiceRequestEvent request)
	{
		ServiceStatus status = new ServiceStatus(ServiceName.INDEX);
		try
		{

			for( ItemIndex<? extends FreetextResult> currentIndex : getAllIndexes() )
			{
				currentIndex.checkHealth();
			}
			status.setServiceStatus(Status.GOOD);
			status.setMoreInfo(
				CurrentLocale.get("com.tle.core.freetext.servicecheck.moreinfo", getIndexPath().getAbsolutePath()));

		}

		catch( Exception e )
		{
			status.setServiceStatus(Status.BAD);
			status.setMoreInfo(e.getMessage());
		}
		eventService.publishApplicationEvent(
			new CheckServiceResponseEvent(request.getRequetserNodeId(), zkService.getNodeId(), status));
	}
}
