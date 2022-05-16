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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;

import com.automatics.constants.AutomaticsConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.enums.ServiceType;
import com.automatics.enums.TestType;
import com.automatics.rdkb.BroadBandTestStepResult;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsUtils;
import com.automatics.utils.CommonMethods;

public class BaseQuickTest extends AutomaticsTestBase {

    private boolean quickTestStarted = false;

    /** The build image name for CDL. */
    protected String buildImageName = null;

    /** Test execution start time. */
    protected long testStartTime = 0L;

    /** Test execution completion time. */
    protected long testCompletionTime = 0L;

    /**
     * The instance variable which holds service name for portal updation.
     */
    protected ServiceType serviceName = ServiceType.FUNCTIONAL_VERIFICATION;

    /**
     * The instance variable which holds test filter type.
     */
    protected TestType testFilterType = TestType.QUICK;

    /**
     * Overall Quick Test status. If any of one box passed we can consider this as passed.
     */
    private static String quickTestStatus = BroadBandTestConstants.QUICK_TEST_FAILURE;

    /**
     * Flag is to check whether we need to update the results back to test manager.
     */
    private static boolean quickTestResultUpdationRequired = false;

    /**
     * Threshold pass percentage to calculate the quick test status for further execution.
     */
    private static float THRESHOLD_PASS_PERCENTAGE = 100.0f;

    /**
     * Holds the number of high priority tests to decide the whether need to proceed with 1 HOUR, 4 HOUR, 2 DAYS, NEW
     * FEATURE execution.
     */
    private static int NUMBER_OF_HIGH_PRIORITY_TEST_STEPS = 5;

    /**
     * JSON Array to hold the list of devices where the Quick test passed.
     */
    private static JSONArray settopList = new JSONArray();

    /** variable to hold the number of passed steps in Quick test **/
    private static Map<String, List<BroadBandTestStepResult>> executionStatus = new ConcurrentHashMap<String, List<BroadBandTestStepResult>>();

    /** Holds the list of failed Settop objects. */
    private List<Dut> failedSettopList = null;

    /**
     * Method to evaluate the execution results on particular devices.
     * 
     * @param params
     *            test parameters.
     */
    @AfterMethod(alwaysRun = true)
    public void evaluateExecutionResults(Object[] params) {

	if (null != params && params.length >= 1 && params[0] instanceof Dut) {

	    if (isQuickTestStarted()) {
		Dut settop = (Dut) params[0];

		double totalTestCases = 0, successTestCount = 0;
		float percentage = 0;

		String failureReason = BroadBandTestConstants.TEST_STATUS_BUILD_VERIFICATION_FAILED;

		List<BroadBandTestStepResult> executionTestStatus = executionStatus.get(settop.getHostMacAddress());

		totalTestCases = NUMBER_OF_HIGH_PRIORITY_TEST_STEPS;
		LOGGER.info("NUMBER OF HIGH PRIORITY TEST RECORD = " + executionTestStatus.size());

		for (BroadBandTestStepResult testStatus : executionTestStatus) {

		    ExecutionStatus testExecutionStatus = testStatus.getStatus();

		    LOGGER.debug("HIGH PRIORITY TEST STEP = " + testStatus.getManualTestId() + " - "
			    + testStatus.getStepNumber() + ", EXECUTION STATUS  = " + testExecutionStatus);

		    if (ExecutionStatus.PASSED.equals(testExecutionStatus)) {
			successTestCount++;
		    } else if (ExecutionStatus.FAILED.equals(testExecutionStatus)) {
			failureReason = testStatus.getErrorMessage();
			LOGGER.error("TEST STEP = " + testStatus.getManualTestId() + " - " + testStatus.getStepNumber()
				+ ", FAILED WITH REASON = " + failureReason);
		    }
		}

		LOGGER.info("NUMBER OF HIGH PRIORITY TEST STEPS PASSED  = " + successTestCount);
		LOGGER.info("NUMBER OF HIGH PRIORITY TEST STEPS =  " + totalTestCases);

		if (totalTestCases > 0 && successTestCount > 0) {
		    percentage = (float) ((successTestCount * 100) / totalTestCases);
		    LOGGER.info("PASS PERCENTAGE = " + percentage);
		}

		if (percentage >= THRESHOLD_PASS_PERCENTAGE) {
		    failureReason = BroadBandTestConstants.TEST_STATUS_BUILD_VERIFICATION_SUCCEEDED;
		    quickTestStatus = BroadBandTestConstants.QUICK_TEST_SUCCESS;
		    settopList.put(settop.getHostMacAddress());
		    quickTestResultUpdationRequired = true;
		} else {
		    if (!quickTestStatus.equalsIgnoreCase(BroadBandTestConstants.QUICK_TEST_SUCCESS)) {
			quickTestStatus = BroadBandTestConstants.QUICK_TEST_FAILURE;
			failureReason = BroadBandTestConstants.TEST_STATUS_BUILD_VERIFICATION_FAILED + " : "
				+ buildImageName;
			LOGGER.error("QUICK TEST FAILURE REASON = " + failureReason);
			quickTestResultUpdationRequired = true;
		    }
		    // Initialize the list
		    if (failedSettopList == null) {
			failedSettopList = new ArrayList<Dut>();
		    }
		    failedSettopList.add(settop);
		}
	    }
	} else {
	    if (!quickTestStatus.equalsIgnoreCase(BroadBandTestConstants.QUICK_TEST_SUCCESS)) {
		quickTestStatus = BroadBandTestConstants.QUICK_TEST_FAILURE_BOX_NOT_AVAILABLE;
		quickTestResultUpdationRequired = true;
	    }
	}

    }

    /**
     * 
     * Method to update the the quick test status to test manager
     * 
     */
    @AfterClass(alwaysRun = true)
    public void updateTestManagerWithStatus() {

	if (quickTestResultUpdationRequired) {

	    testCompletionTime = System.currentTimeMillis();

	    long executionTime = (testCompletionTime - testStartTime) / BroadBandTestConstants.ONE_MINUTE_IN_MILLIS;

	    LOGGER.info(new StringBuilder().append("BUILD NAME  =  ").append(buildImageName)
		    .append(" | SERVICE TYPE = ").append(serviceName.get()).append(" | FILTER TEST TYPE = ")
		    .append(testFilterType.get()).append(" | TEST START TIME = ").append(testStartTime)
		    .append(" | TEST COMPLETION TIME = ").append(testCompletionTime)
		    .append(" | EXECUTION TIME IN MINUTES = ").append(executionTime).toString());

	    JSONObject jsonToBePassed = new JSONObject();

	    try {

		jsonToBePassed.put(JSON_KEY_OBJECT_BUILD_IMAGE_NAME, buildImageName);
		jsonToBePassed.put(JSON_KEY_OBJECT_STATUS, quickTestStatus);
		jsonToBePassed.put(JSON_KEY_OBJECT_SETTOP_LIST, settopList);
		jsonToBePassed.put(JSON_KEY_OBJECT_START_TIME, testStartTime);
		jsonToBePassed.put(JSON_KEY_OBJECT_COMPLETION_TIME, testCompletionTime);
		jsonToBePassed.put(AutomaticsConstants.JOB_MANAGER_DETAILS_ID,
			Integer.parseInt(System.getProperty(AutomaticsConstants.JOB_MANAGER_DETAILS_ID, "0")));
		jsonToBePassed.put(AutomaticsConstants.SYSTEM_PROPERTY_UPDATE_RDK_PORTAL, Boolean.parseBoolean(
			System.getProperty(AutomaticsConstants.SYSTEM_PROPERTY_UPDATE_RDK_PORTAL, "false")));
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

    /**
     * Method to perform some quick initialization before starting Quick Test.
     */
    @BeforeClass
    public void setupBuildDetails() {
	String testBuildName = System.getProperty(AutomaticsConstants.BUILD_NAME_SYSTEM_PROPERTY);
	if (CommonMethods.isNotNull(testBuildName)) {
	    testStartTime = System.currentTimeMillis();
	    serviceName = ServiceType.valueOf(System.getProperty(AutomaticsConstants.SERVICE_NAME, serviceName.get()));
	    testFilterType = TestType.valueOf(
		    System.getProperty(AutomaticsConstants.SYSTEM_PROPERTY_FILTER_TEST_TYPE, testFilterType.get()));
	    buildImageName = CommonMethods.extractBuildNameWithoutExtension(testBuildName);

	    LOGGER.info(new StringBuilder().append("BUILD NAME  =  ").append(buildImageName)
		    .append(" | SERVICE TYPE = ").append(serviceName.get()).append(" | FILTER TEST TYPE = ")
		    .append(testFilterType.get()).append(" | TEST START TIME = ").append(testStartTime).toString());

	}
    }

    /**
     * @return the quickTestStarted
     */
    protected boolean isQuickTestStarted() {
	return quickTestStarted;
    }

    /**
     * @param quickTestStarted
     *            the quickTestStarted to set
     */
    protected void setQuickTestStarted(boolean quickTestStarted) {
	this.quickTestStarted = quickTestStarted;
    }

    /**
     * Helper method to update the execution results.
     * 
     * @param device
     *            The device to be used for testing.
     * @param testId
     *            The manual Test ID.
     * @param step
     *            The manual test step number.
     * @param status
     *            The execution status.
     * @param errorMessage
     *            The Error message.
     * @param blockExec
     *            The flag to block the execution.
     */
    protected void updateExecutionStatus(Dut device, String testId, String step, boolean status, String errorMessage,
	    boolean blockExec) {

	ExecutionStatus exeStatus = status ? ExecutionStatus.PASSED : ExecutionStatus.FAILED;

	String deviceMac = device.getHostMacAddress();
	List<BroadBandTestStepResult> results = null;
	BroadBandTestStepResult stepResult = new BroadBandTestStepResult(deviceMac, testId, step, exeStatus,
		errorMessage);

	if (executionStatus.containsKey(deviceMac)) {
	    results = executionStatus.get(deviceMac);
	    results.add(stepResult);
	} else {
	    results = new ArrayList<BroadBandTestStepResult>();
	    results.add(stepResult);
	}
	executionStatus.put(deviceMac, results);
	tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, blockExec);
    }
}
