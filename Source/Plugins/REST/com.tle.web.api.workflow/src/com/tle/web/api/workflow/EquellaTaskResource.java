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

package com.tle.web.api.workflow;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.tle.web.api.item.tasks.interfaces.TaskResource;
import com.wordnik.swagger.annotations.Api;
import com.wordnik.swagger.annotations.ApiOperation;
import com.wordnik.swagger.annotations.ApiParam;

/**
 * @author Aaron
 */
@Path("task")
@Api(value = "/task", description = "task")
@Produces(MediaType.APPLICATION_JSON)
public interface EquellaTaskResource extends TaskResource
{
	@GET
	@Path("/filter")
	@ApiOperation(value = "Get the counts for each task type")
	public Response getTaskFilters(
		// @formatter:off
		@ApiParam(value = "Do not return task filters that contain no tasks. Implies includeCounts=true.", allowableValues = "true,false", defaultValue = "false", required = false)
		@QueryParam("ignoreZero")
			boolean ignoreZeroStr,
		@ApiParam(value = "Include task counts against each filter name.", allowableValues = "true,false", defaultValue = "false", required = false)
		@QueryParam("includeCounts")
			boolean includeCounts
		);
		// @formatter:on
}
