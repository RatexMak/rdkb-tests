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

public class ParodusClientReconnectTest extends AutomaticsTestBase{
    /**
     * Verify client re-connection by forcefully killing parodus and let it restart through self heal
     * <ol>
     * <li>Verify parodus restarted by self heal when killed</li>
     * <li>Verify parodus reconnected to server after restart</li>
     * <li>Verify webpa reconnected after parodus restart</li>
     * </ol>
     * 
     * @author Ashwin sankara
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WEBPA)
    @TestDetails(testUID = "TC-RDKB-PARODUS-RECON-1001")
    public void testVerifyParodusRestartClientsReconnect(Dut device) {

	// Variable Declaration begins
	// String to store test case id
	String testCaseId = "TC-RDKB-PARODUS-RECON-001";
	// String to store step number
	String stepNum = "s1";
	// String to store error message
	String errorMessage = "";
	// boolean to store step result
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-PARODUS-RECON-1001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify client re-connection by forcefully killing parodus and let it restart through self heal");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify parodus restarted by self heal when killed");
	LOGGER.info("2. Verify parodus reconnected to server after restart");
	LOGGER.info("3. Verify webpa reconnected after parodus restart");
	LOGGER.info("#######################################################################################");
	try {
	    errorMessage = "Failed to restart parodus process with kill -11 command";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify parodus restarted by self heal when killed");
	    LOGGER.info("STEP 1: ACTION : Execute commands:pidof paroduskill -11 paroduspidof parodus");
	    LOGGER.info("STEP 1: EXPECTED : Obtained new pid for parodus after restart");
	    LOGGER.info("**********************************************************************************");
            try{
	   	 tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PARODUSLOGS_NVRAM);
            }catch(Exception e){
           	 LOGGER.error("STEP 1: ACTUAL: Exception occurred while creating paroduslog under /nvram");
            }
	    status = BroadBandSystemUtils.killAndVerifyProcessRestarted(device, tapEnv,
		    BroadBandTestConstants.PROCESS_NAME_PARODUS);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Obtained new pid for parodus after restart");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Unable to find connected to server log message after parodus restart";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify parodus reconnected to server after restart");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute command:grep \"Connected to server over SSL\" /rdklogs/logs/PARODUSlog.txt.0");
	    LOGGER.info("STEP 2: EXPECTED : Parodus connected to server log message present after restart");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_SERVER_CONNECTION, BroadBandCommandConstants.LOG_FILE_PARODUS,
		    BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Parodus connected to server log message present after restart");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Unable to find Config client registered log message after parodus restart";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify webpa reconnected after parodus restart");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute command:grep -i \" Client config Registered successfully\" /rdklogs/logs/PARODUSlog.txt.0");
	    LOGGER.info("STEP 3: EXPECTED : Config client registered log message present after parodus restart");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_CONFIG_CLIENT_REGISTERED,
		    BroadBandCommandConstants.LOG_FILE_PARODUS, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Config client registered log message present after parodus restart");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-PARODUS-RECON-1001");
	LOGGER.info("#######################################################################################");
    }
}
