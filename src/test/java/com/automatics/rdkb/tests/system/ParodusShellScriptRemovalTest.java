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

package com.automatics.rdkb.tests.system;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandSystemUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

/**
 * Class for Killing the parodus process on arm and verify process is getting started during next self heal window.
 */

public class ParodusShellScriptRemovalTest extends AutomaticsTestBase {

    /**
     * Test case is created as part of NEW FEATURE AUTOMATION based on the Removal of parodus start shell script changes
     * from yocto
     *
     * Test Case # 1: Verify the parodus process should be running. Kill parodus process running and verify process is
     * getting started during next self heal window.
     *
     * <p>
     * STEPS:
     * <ol>
     * <li>S1)Verify killing the parodus process.</li>
     * <li>S2)verify the parodus process is getting started during next selfheal window.</li>
     * <li>S3)verify the parodus process is running after getting started in selfheal window</li>
     * </ol>
     * 
     * @author JOSEPH M
     * @Refactor Alan_Bivera
     * 
     * @param device
     *            {@link Dut}
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, groups = {
	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.SYSTEM })
    @TestDetails(testUID = "TC-RDKB-PARODUS-1001")
    public void removeParodus(Dut device) {

	String testCaseId = "TC-RDKB-PARODUS-101";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-PARODUS-1001");
	LOGGER.info(
		"TEST DESCRIPTION: Test case is created as part of NEW FEATURE AUTOMATION based on the Removal of parodus start shell script changes\r\n from yocto");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE CONDITION 1. Verify the parodus process is running.");
	LOGGER.info("1. Verify killing the parodus process on arm console.");
	LOGGER.info("2. Verify the parodus process is getting started during next self heal window.");
	LOGGER.info("3. Verify the parodus process is running after getting started in selfheal window");

	LOGGER.info("#######################################################################################");
	boolean result = false;
	String errorMessage = null;
	String step = "s1";
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRECONDITION: Verify the parodus process is running.");
	    String response = CommonUtils.getPidOfProcess(device, tapEnv, BroadBandTestConstants.PARODUS_PROCESS_NAME);
	    result = CommonMethods.isNotNull(response);
	    errorMessage = "PARODUS PROCESS IS NOT RUNNING";

	    if (!result) {

		throw new Exception(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    LOGGER.info("#######################################################################################");
	    step = "s1";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify killing the parodus process on arm console.");
	    LOGGER.info("STEP 1: ACTION : ");
	    LOGGER.info("STEP 1: EXPECTED : PARODUS PROCESS MUST BE KILLED");
	    LOGGER.info("**********************************************************************************");
	    String timeStamp = BroadBandCommonUtils.getCurrentTimeStampOnDevice(tapEnv, device);
	    CommonUtils.killTheProcess(device, tapEnv, BroadBandTestConstants.PARODUS_PROCESS_NAME);
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    String pidFinal = CommonUtils.getPidOfProcess(device, tapEnv, BroadBandTestConstants.PARODUS_PROCESS_NAME);
	    result = CommonMethods.isNull(pidFinal);
	    errorMessage = "PARODOUS PROCESS IS NOT RUNNING";
	    LOGGER.info("S1 - ACTUAL: " + (result ? "parodus process is killed" : errorMessage));
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    step = "s2";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify the parodus process is getting started during next self heal window.");
	    LOGGER.info("STEP 2: ACTION : ");
	    LOGGER.info("STEP 2: EXPECTED : PARODUS SHOULD GET STARTED");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSystemUtils.verifyArmConsoleLogForPollingTime(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_PARODUS_PROCESS, BroadBandCommandConstants.FILE_SELFHEAL_LOG,
		    timeStamp, BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS);
	    errorMessage = "PARODUS PROCESS IS NOT GETTING STARTED";
	    LOGGER.info("S2 - ACTUAL: "
		    + (result ? "parodus process is getting started during the next self heal window" : errorMessage));
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);

	    step = "s3";
	    result = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify the parodus process is running after getting started in selfheal window.");
	    LOGGER.info("STEP 3: ACTION : ");
	    LOGGER.info("STEP 3: EXPECTED : PARODUS SHOULD BE RUNNING");
	    LOGGER.info("**********************************************************************************");
	    response = CommonUtils.getPidOfProcess(device, tapEnv, BroadBandTestConstants.PARODUS_PROCESS_NAME);
	    result = CommonMethods.isNotNull(response);
	    errorMessage = "PARODUS PROCESS IS NOT RUNNING";
	    LOGGER.info("S3 - ACTUAL: " + (result ? "parodus process is running" : errorMessage));
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);
	}

	catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING THE PARODUS PROCESS " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
	}
    }
}
