/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.quick;

import java.util.HashMap;
import java.util.Map;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.AfterClass;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ServiceType;
import com.automatics.enums.TestType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsUtils;
import com.automatics.utils.TestUtils;

public class DummyQuickTest extends AutomaticsTestBase {

	/**
	 * Overall Quick Test status. If any of one box passed we can consider this as
	 * passed.
	 */
	private static Map<String, String> deviceStatus = new HashMap<String, String>();

	/** Test execution start time. */
	protected long testStartTime = 0L;

	/** Test execution completion time. */
	protected long testCompletionTime = 0L;

	/** The build image name for CDL. */
	protected String buildImageName = null;

	/**
	 * The instance variable which holds test filter type.
	 */
	protected TestType testFilterType = TestType.QUICK;

	/**
	 * Overall Quick Test status. If any of one box passed we can consider this as
	 * passed.
	 */
	private static String quickTestStatus = "FAILURE";

	/**
	 * The instance variable which holds service name for portal updation.
	 */
	protected ServiceType serviceName = ServiceType.FUNCTIONAL_VERIFICATION;

	/**
	 * Method to perform some quick initialization before starting Quick Test.
	 */
	@BeforeClass
	public void setupBuildDetails() {
		testStartTime = System.currentTimeMillis();
		serviceName = ServiceType.valueOf(System.getProperty(AutomaticsConstants.SERVICE_NAME, serviceName.get()));
		testFilterType = TestType.valueOf(
				System.getProperty(AutomaticsConstants.SYSTEM_PROPERTY_FILTER_TEST_TYPE, testFilterType.get()));
		buildImageName = TestUtils.getBuildName();
	}

	/**
	 * Perform dummy CI quick test
	 * 
	 * @param device
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-QUICK-CI-DUMMY-ALL-MODELS")
	public void testDummyCiQuick(Dut device) {
		String testCaseId = "TC-RDKB-QUICK-CI-DUMMY-ALL-MODELS";
		String stepNum = "s1";
		boolean passStatus = true;
		LOGGER.info("Dummy CI Quick test");
		// Update verification status to Automatics
		deviceStatus.put(device.getHostMacAddress(), "SUCCESS");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, passStatus, "", false);
	}

	/**
	 * 
	 * Method to update the the quick test status to Automatics
	 * 
	 */
	@AfterClass(alwaysRun = true)
	public void updateAutomaticsWithStatus() {
		testCompletionTime = System.currentTimeMillis();
		long executionTime = (testCompletionTime - testStartTime) / AutomaticsConstants.ONE_MINUTE_IN_MILLIS;
		LOGGER.info(new StringBuilder().append("BUILD NAME  =  ").append(buildImageName).append(" | SERVICE TYPE = ")
				.append(serviceName.get()).append(" | FILTER TEST TYPE = ").append(testFilterType.get())
				.append(" | TEST START TIME = ").append(testStartTime).append(" | TEST COMPLETION TIME = ")
				.append(testCompletionTime).append(" | EXECUTION TIME IN MINUTES = ").append(executionTime).toString());
		JSONObject jsonToBePassed = new JSONObject();
		JSONArray successMacs = new JSONArray();
		int successCount = 0;
		for (String mac : deviceStatus.keySet()) {
			if ("SUCCESS".equals(deviceStatus.get(mac))) {
				LOGGER.info("QT success for device: {}", mac);
				successCount++;
				successMacs.put(mac);
			} else {
				LOGGER.info("QT failed for device: {}", mac);
			}
		}
		if (successCount > 0) {
			quickTestStatus = "COMPLETED";
		} else {
			quickTestStatus = "FAILURE";
		}
		try {
			jsonToBePassed.put(JSON_KEY_OBJECT_BUILD_IMAGE_NAME, buildImageName);
			jsonToBePassed.put(JSON_KEY_OBJECT_STATUS, quickTestStatus);
			jsonToBePassed.put(JSON_KEY_OBJECT_SETTOP_LIST, successMacs);
			jsonToBePassed.put(JSON_KEY_OBJECT_START_TIME, testStartTime);
			jsonToBePassed.put(JSON_KEY_OBJECT_COMPLETION_TIME, testCompletionTime);
			jsonToBePassed.put(AutomaticsConstants.JOB_MANAGER_DETAILS_ID,
					Integer.parseInt(System.getProperty(AutomaticsConstants.JOB_MANAGER_DETAILS_ID, "0")));
			jsonToBePassed.put(AutomaticsConstants.SYSTEM_PROPERTY_UPDATE_RDK_PORTAL, Boolean
					.parseBoolean(System.getProperty(AutomaticsConstants.SYSTEM_PROPERTY_UPDATE_RDK_PORTAL, "false")));
			jsonToBePassed.put("service", serviceName.get());
			JSONObject result = new JSONObject();
			result.put(JSON_KEY_OBJECT_BUILD_NAME, buildImageName);
			result.put(JSON_KEY_OBJECT_TYPE, testFilterType.get());
			result.put(JSON_KEY_OBJECT_TESTS, new JSONArray());
			jsonToBePassed.put("result", result);
		} catch (JSONException jex) {
			LOGGER.error("Test Results formation failed ", jex);
		}
		AutomaticsUtils.jsonToAutomatics = jsonToBePassed.toString();
	}
}